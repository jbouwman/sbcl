;;;; Tests for sb-fiber (impure -- GC stress, concurrency, performance)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(unless (and (member :sb-thread *features*)
             (or (member :x86-64 *features*) (member :arm64 *features*))
             (not (member :win32 *features*)))
  (invoke-restart 'run-tests::skip-file))

(require :sb-fiber)
(use-package :sb-fiber)

;;; --- Stage 4 tests: GC integration ---

(with-test (:name (:fiber :gc-with-new-fibers))
  ;; Create fibers (NEW state) and trigger GC
  (let ((fibers (loop repeat 100
                      collect (make-fiber
                               (lambda ()
                                 (make-array 10 :initial-element 42))))))
    (sb-ext:gc :full t)
    (sb-ext:gc :full t)
    (assert (every #'fiber-alive-p fibers))
    (mapc #'destroy-fiber fibers)))

(with-test (:name (:fiber :gc-with-suspended-fibers))
  ;; Fibers that have yielded (RUNNABLE state) hold live references on stack
  (let* ((main (make-main-fiber))
         (n 50)
         (fibers (make-array n))
         (results (make-array n :initial-element nil)))
    ;; Create fibers that allocate objects and yield
    (dotimes (i n)
      (let ((idx i))
        (setf (aref fibers i)
              (make-fiber
               (lambda ()
                 ;; Allocate objects that live only on this fiber's stack
                 (let ((data (list (make-array 5 :initial-element idx)
                                   (cons idx (* idx idx))
                                   (format nil "fiber-~D" idx))))
                   (fiber-switch (aref fibers idx) main)
                   ;; After resuming, verify objects survived GC
                   (setf (aref results idx)
                         (and (= (aref (first data) 0) idx)
                              (= (car (second data)) idx)
                              (string= (third data)
                                       (format nil "fiber-~D" idx))))
                   (fiber-switch (aref fibers idx) main)))))))
    ;; Switch to each fiber to get them into RUNNABLE state
    (dotimes (i n)
      (fiber-switch main (aref fibers i)))
    ;; Now trigger GC while fibers are suspended with live objects
    (sb-ext:gc :full t)
    (sb-ext:gc :full t)
    (sb-ext:gc :full t)
    ;; Resume each fiber -- they check their objects survived
    (dotimes (i n)
      (fiber-switch main (aref fibers i)))
    ;; Verify all fibers found their objects intact
    (dotimes (i n)
      (assert (aref results i) ()
              "Fiber ~D: objects corrupted after GC" i))
    (dotimes (i n) (destroy-fiber (aref fibers i)))
    (destroy-fiber main)))

(with-test (:name (:fiber :gc-during-repeated-switches))
  ;; Interleave GC with fiber switching
  (let* ((main (make-main-fiber))
         (child nil)
         (switch-count 0))
    (setf child
          (make-fiber
           (lambda ()
             (loop
               (incf switch-count)
               ;; Allocate garbage to trigger GC pressure
               (make-array 1000)
               (fiber-switch child main)))))
    (dotimes (i 200)
      (fiber-switch main child)
      (when (zerop (mod i 20))
        (sb-ext:gc :full t)))
    (assert (= switch-count 200))
    (destroy-fiber child)
    (destroy-fiber main)))

;;; --- Mass creation/destruction ---

(with-test (:name (:fiber :mass-create-destroy))
  (dotimes (i 1000)
    (let ((f (make-fiber (lambda ()))))
      (destroy-fiber f)))
  (assert t))

(with-test (:name (:fiber :gc-during-mass-creation))
  (let ((fibers nil))
    (dotimes (i 500)
      (push (make-fiber (lambda ())) fibers)
      (when (zerop (mod i 50))
        (sb-ext:gc :full t)))
    (mapc #'destroy-fiber fibers)))

;;; --- Binding stack stress ---

(defvar *binding-test-var* :default)

(with-test (:name (:fiber :binding-stack-stress))
  ;; Multiple fibers each bind the same special variable to different values
  (let* ((main (make-main-fiber))
         (n 20)
         (fibers (make-array n))
         (results (make-array n :initial-element nil)))
    (dotimes (i n)
      (let ((idx i))
        (setf (aref fibers i)
              (make-fiber
               (lambda ()
                 (let ((*binding-test-var* idx))
                   ;; Yield multiple times with binding active
                   (dotimes (round 10)
                     (fiber-switch (aref fibers idx) main)
                     ;; Verify binding survived the switch
                     (unless (= *binding-test-var* idx)
                       (setf (aref results idx) :failed)
                       (return)))
                   (setf (aref results idx) :ok))
                 (fiber-switch (aref fibers idx) main))))))
    ;; Drive all fibers through their 10 rounds
    (dotimes (round 10)
      (dotimes (i n)
        (fiber-switch main (aref fibers i)))
      ;; Verify main thread's binding is still :default
      (assert (eq *binding-test-var* :default) ()
              "Main thread binding corrupted in round ~D" round))
    ;; Let fibers finish
    (dotimes (i n)
      (fiber-switch main (aref fibers i)))
    (dotimes (i n)
      (assert (eq (aref results i) :ok) ()
              "Fiber ~D binding test failed: ~S" i (aref results i)))
    (dotimes (i n) (destroy-fiber (aref fibers i)))
    (destroy-fiber main)))

;;; --- IMPL-344 handler-chain regressions ---
;;;
;;; Two shapes of the original bug: (1) nested handler-case across a
;;; switch silently skips the inner handler (root cause: binding-
;;; stack *handler-clusters* tail pointed into another fiber's bs);
;;; (2) a condition that escapes the fiber's entry function segfaults
;;; at offset 0x18 instead of propagating cleanly.  Both were fixed
;;; by the swap_one indirect-cell maintenance in fiber.c and by the
;;; handler-case in sb-fiber-lisp-entry that catches escapes and
;;; re-signals them in the caller via FIBER-SWITCH.  Plus a 1000-
;;; iteration stress to guard against intermittent re-introduction.

#+sb-thread
(with-test (:name (:fiber :impl-344 :nested-handler-across-switch))
  (let ((sem (sb-thread:make-semaphore))
        (user-caught nil)
        (outer-caught nil))
    (sb-thread:make-thread
     (lambda ()
       (let* ((mf (make-main-fiber))
              (f nil))
         (setf f
               (make-fiber
                (lambda ()
                  (handler-case                       ; outer
                      (handler-case                   ; inner
                          (progn
                            (fiber-switch f mf)
                            (error "boom"))
                        (error (e)
                          (setf user-caught (princ-to-string e))))
                    (error (e)
                      (setf outer-caught (princ-to-string e)))))))
         (fiber-switch mf f)
         (fiber-switch mf f)
         (destroy-fiber f)
         (destroy-fiber mf))
       (sb-thread:signal-semaphore sem))
     :name "impl-344-reproducer-1")
    (assert (sb-thread:wait-on-semaphore sem :timeout 5))
    (assert (equal user-caught "boom"))
    (assert (null outer-caught))))

(with-test (:name (:fiber :impl-344 :escaping-condition-propagates))
  (let* ((mf (make-main-fiber))
         (f (make-fiber (lambda () (error "minimal boom"))))
         (caught nil))
    (handler-case
        (fiber-switch mf f)
      (error (e)
        (setf caught (princ-to-string e))))
    (destroy-fiber f)
    (destroy-fiber mf)
    (assert (equal caught "minimal boom"))))

(with-test (:name (:fiber :impl-344 :nested-handler-stress))
  (dotimes (i 1000)
    (let* ((mf (make-main-fiber))
           (caught nil)
           (f (make-fiber
               (lambda ()
                 (handler-case
                     (handler-case
                         (progn
                           (fiber-switch sb-fiber::*current-fiber* mf)
                           (error "iter ~A" i))
                       (error (e)
                         (setf caught (princ-to-string e))))
                   (error () nil))))))
      (fiber-switch mf f)
      (fiber-switch mf f)
      (assert (search (format nil "iter ~A" i) (or caught "")))
      (destroy-fiber f)
      (destroy-fiber mf))))

;;; --- Concurrency: fibers on multiple OS threads ---

#+sb-thread
(with-test (:name (:fiber :multi-thread-independent-fibers))
  ;; Each OS thread gets its own set of fibers -- no cross-thread sharing
  (let ((threads nil)
        (results (make-array 4 :initial-element nil))
        (lock (sb-thread:make-mutex :name "results")))
    (dotimes (ti 4)
      (let ((thread-idx ti))
        (push
         (sb-thread:make-thread
          (lambda ()
            (let* ((main (make-main-fiber))
                   (count 0)
                   (child nil))
              (setf child
                    (make-fiber
                     (lambda ()
                       (dotimes (i 1000)
                         (incf count)
                         (fiber-switch child main)))))
              (dotimes (i 1000)
                (fiber-switch main child))
              (destroy-fiber child)
              (destroy-fiber main)
              (sb-thread:with-mutex (lock)
                (setf (aref results thread-idx) count))))
          :name (format nil "fiber-test-~D" ti))
         threads)))
    (mapc #'sb-thread:join-thread threads)
    (dotimes (i 4)
      (assert (= (aref results i) 1000) ()
              "Thread ~D: expected 1000 switches, got ~S" i (aref results i)))))

;;; --- Stack overflow detection ---

;;; Deep recursion on a fiber stack trips the SOFT guard page; the
;;; signal handler lowers it and dispatches CONTROL-STACK-EXHAUSTED-
;;; ERROR, which signals a STORAGE-CONDITION.  The fiber's handler-
;;; case catches it and yields normally; when the fiber later unwinds
;;; past the recovered frame, the RETURN guard re-traps and re-
;;; protects the SOFT guard for the next overflow attempt.  Uses
;;; non-tail recursion so the compiler cannot optimize the call away.
;;;
;;; The test harness runs impure tests in a child SBCL with
;;; --lose-on-corruption, which turns the SOFT-guard path into
;;; LOSE() instead of the recoverable return-to-
;;; CONTROL-STACK-EXHAUSTED-ERROR path.  Temporarily flip
;;; lose_on_corruption_p off around the overflow probe so the
;;; guard fires in its normal recoverable form; flip it back on as
;;; soon as we're past the handler-case so the rest of the test
;;; file enjoys the usual safety.
(sb-alien:define-alien-variable ("lose_on_corruption_p" %lose-on-corruption-p)
    sb-alien:int)

(with-test (:name (:fiber :stack-overflow-detection))
  (let* ((main (make-main-fiber))
         (overflow-caught nil)
         (child nil)
         (saved-flag %lose-on-corruption-p))
    (setf child
          (make-fiber
           (lambda ()
             (declare (optimize (speed 0) (safety 3) (debug 3)))
             (handler-case
                 (labels ((blow-stack (n)
                            (1+ (blow-stack (1+ n)))))
                   (blow-stack 0))
               (storage-condition ()
                 (setf overflow-caught t)))
             (fiber-switch child main))
           :stack-size 65536))
    (unwind-protect
         (progn
           (setf %lose-on-corruption-p 0)
           (fiber-switch main child))
      (setf %lose-on-corruption-p saved-flag))
    (assert overflow-caught () "Stack overflow was not caught")
    (destroy-fiber child)
    (destroy-fiber main)))

;;; --- Simulated workload ---

(with-test (:name (:fiber :simulated-io-workload))
  ;; Simulate the motivating use case: multiple "connections" each doing
  ;; multi-step work with yields between steps
  (let* ((main (make-main-fiber))
         (n-connections 100)
         (n-steps 5)
         (fibers (make-array n-connections))
         (completed (make-array n-connections :initial-element nil)))
    (dotimes (i n-connections)
      (let ((idx i))
        (setf (aref fibers i)
              (make-fiber
               (lambda ()
                 ;; Simulate a multi-step connection handler
                 (let ((buf (make-array 256 :element-type '(unsigned-byte 8)
                                            :initial-element 0)))
                   (dotimes (step n-steps)
                     ;; "Process" step: write to buffer
                     (dotimes (j 256)
                       (setf (aref buf j) (mod (+ idx step j) 256)))
                     ;; "Yield" to scheduler
                     (fiber-switch (aref fibers idx) main))
                   ;; Verify final buffer state
                   (setf (aref completed idx)
                         (= (aref buf 0) (mod (+ idx (1- n-steps) 0) 256))))
                 (fiber-switch (aref fibers idx) main))))))
    ;; Simple round-robin scheduler
    (let ((active (loop for i below n-connections collect i)))
      (loop while active do
        (let ((next-active nil))
          (dolist (i active)
            (fiber-switch main (aref fibers i))
            ;; If fiber is still alive, keep it in the active list
            (unless (aref completed i)
              (push i next-active)))
          (setf active (nreverse next-active)))))
    ;; Let all fibers do their final yield
    (dotimes (i n-connections)
      (unless (aref completed i)
        (fiber-switch main (aref fibers i))))
    ;; Verify all completed
    (let ((ok-count (count t completed)))
      (assert (= ok-count n-connections) ()
              "Only ~D/~D connections completed successfully" ok-count n-connections))
    (dotimes (i n-connections) (destroy-fiber (aref fibers i)))
    (destroy-fiber main)))

;;; --- Fan-out scaling (Stage 6 tests 2+3) ---

(with-test (:name (:fiber :fanout-scaling))
  ;; Throughput with N fibers, round-robin on 1 thread.
  ;; Validates that switching overhead scales linearly, not quadratically.
  (let ((targets '(10 100 1000 10000)))
    (dolist (n targets)
      (let* ((main (make-main-fiber))
             (fibers (make-array n))
             (total 0))
        (dotimes (i n)
          (let ((idx i))
            (setf (aref fibers i)
                  (make-fiber
                   (lambda ()
                     (loop
                       (incf total)
                       (fiber-switch (aref fibers idx) main)))))))
        (let* ((rounds (max 1 (floor 100000 n)))
               (start (get-internal-real-time)))
          (dotimes (r rounds)
            (dotimes (i n)
              (fiber-switch main (aref fibers i))))
          (let* ((elapsed (/ (- (get-internal-real-time) start)
                             internal-time-units-per-second))
                 (switches (* rounds n))
                 (rate (if (> elapsed 0) (/ switches elapsed) 0)))
            (format t "~&;   ~6D fibers x ~D rounds: ~,2F switches/sec (~,1F ns/switch)~%"
                    n rounds rate (if (> rate 0) (/ 1.0e9 rate) 0))))
        (assert (> total 0) () "No switches executed for N=~D" n)
        (dotimes (i n) (destroy-fiber (aref fibers i)))
        (destroy-fiber main)))))

(with-test (:name (:fiber :fanout-gc-under-load))
  ;; GC pause with N live fibers holding live objects.
  ;; Each fiber allocates objects, yields, and verifies them post-GC.
  (dolist (n '(100 1000 5000))
    (let* ((main (make-main-fiber))
           (fibers (make-array n))
           (ok (make-array n :initial-element nil)))
      (dotimes (i n)
        (let ((idx i))
          (setf (aref fibers i)
                (make-fiber
                 (lambda ()
                   ;; Allocate objects that live on the fiber's stack
                   (let ((data (list (cons idx idx)
                                     (make-array 3 :initial-element idx))))
                     (fiber-switch (aref fibers idx) main)
                     ;; Verify objects survived GC
                     (setf (aref ok idx)
                           (and (= (car (first data)) idx)
                                (= (aref (second data) 0) idx)))
                     (fiber-switch (aref fibers idx) main)))))))
      ;; Switch to all fibers to get them into RUNNABLE with live objects
      (dotimes (i n)
        (fiber-switch main (aref fibers i)))
      ;; Trigger full GC
      (let ((start (get-internal-real-time)))
        (sb-ext:gc :full t)
        (let* ((elapsed (/ (- (get-internal-real-time) start)
                           internal-time-units-per-second)))
          (format t "~&;   ~6D fibers: GC pause ~,3F sec~%" n elapsed)))
      ;; Resume all fibers to verify
      (dotimes (i n)
        (fiber-switch main (aref fibers i)))
      (let ((pass-count (count t ok)))
        (assert (= pass-count n) ()
                "GC corruption: ~D/~D fibers OK at N=~D" pass-count n n))
      (dotimes (i n) (destroy-fiber (aref fibers i)))
      (destroy-fiber main))))

;;; --- Caller-frame liveness across a high-switch-count loop ---
;;;
;;; Regression test for a GC scanning bug (fixed 2026-04-22) where
;;; roots held only in the caller's frame -- above the frame that
;;; drives a high-rate fiber-switch loop -- could be collected.  Two
;;; independent causes fed the same failure mode: (1) main fiber's
;;; Lisp stack on x86-64 was not scanned while suspended because
;;; scan_fiber_stacks gated on stack_end != NULL, (2) the fiber state
;;; flip in the Lisp shim set to.state = RUNNING before the C-side
;;; th->control_stack_* swap, leaving a window where neither scanner
;;; covered to's stack.  The corruption manifested as SAP objects
;;; allocated on top of cons cells the runner held for its test-name
;;; argument, crashing PRINT-OBJECT for SAP at offset 0x12a.
(defun %fiber-caller-frame-body ()
  (let* ((main (make-main-fiber))
         (child (make-fiber
                 (lambda ()
                   (loop (fiber-switch sb-fiber::*current-fiber* main))))))
    (dotimes (i 1000000) (fiber-switch main child))
    ;; Force an alien call (invokes INVOKE-WITH-SAVED-FP) after the
    ;; loop -- this is the shape that tripped the original crash.
    (let ((x (get-internal-real-time))) (declare (ignore x)))
    (destroy-fiber child)
    (destroy-fiber main)))

(defun %fiber-caller-frame-wrap (body name)
  (funcall body)
  ;; NAME must still be a valid cons here; printing dereferences it.
  (assert (consp name))
  (assert (eq (first name) :fiber))
  (assert (eq (second name) :liveness-probe)))

(with-test (:name (:fiber :caller-frame-live-across-1m-switches))
  (%fiber-caller-frame-wrap #'%fiber-caller-frame-body
                            (list :fiber :liveness-probe)))

;;; --- Throughput measurement (informational, not pass/fail) ---

(with-test (:name (:fiber :switch-throughput))
  (let* ((main (make-main-fiber))
         (child nil)
         (n 1000000))
    (setf child
          (make-fiber
           (lambda ()
             (loop (fiber-switch child main)))))
    (let ((start (get-internal-real-time)))
      (dotimes (i n)
        (fiber-switch main child))
      (let* ((elapsed (/ (- (get-internal-real-time) start)
                         internal-time-units-per-second))
             (rate (/ n elapsed)))
        (format t "~&; Fiber switch throughput (C path): ~,2F switches/sec (~,1F ns/switch)~%"
                rate (/ 1.0e9 rate))))
    (destroy-fiber child)
    (destroy-fiber main)))

;;; --- Per-thread freelist -------------------------------------------------
;;;
;;; sb_fiber_destroy parks default-sized idle fibers on the owning
;;; thread's freelist instead of munmap'ing them, and sb_fiber_create
;;; picks them up on the next default-sized make-fiber.  The tests here
;;; cover: pool reuse actually happens, GC is safe across pooled
;;; fibers, and non-default sizes still take the unpooled path.

#+linux
(with-test (:name (:fiber :freelist-reuse-no-mmap-growth))
  (labels ((mapcount ()
             (with-open-file (s "/proc/self/maps")
               (loop for line = (read-line s nil) while line count t))))
    ;; Warm the pool past its cap so the loop below exercises the
    ;; pool-hit fast path exclusively.
    (dotimes (i 40) (destroy-fiber (make-fiber (lambda ()))))
    (let ((before (mapcount)))
      (dotimes (i 10000) (destroy-fiber (make-fiber (lambda ()))))
      (let ((grew (- (mapcount) before)))
        ;; Pool is bounded; additional make-fiber must hit the pool
        ;; and reuse the same mmap'd regions.  Small slack for
        ;; unrelated allocator churn.
        (assert (< grew 8)
                () "Pool not reused: /proc/self/maps grew by ~D lines" grew)))))

(with-test (:name (:fiber :gc-with-pooled-fibers))
  ;; Create/destroy churn interleaved with GC.  Pooled fibers are
  ;; unregistered from fiber_list and state=DEAD, so GC must skip
  ;; them cleanly.
  (dotimes (i 200)
    (destroy-fiber (make-fiber (lambda () (make-array 10))))
    (when (zerop (mod i 25))
      (sb-ext:gc :full t))))

(with-test (:name (:fiber :non-default-size-bypasses-pool))
  ;; Custom-size fibers are ineligible for pooling and must still
  ;; round-trip through the full mmap/munmap path without error.
  (let ((f (make-fiber (lambda ()) :stack-size (* 128 1024))))
    (assert (fiber-alive-p f))
    (destroy-fiber f)
    (assert (not (fiber-alive-p f)))))

;;; --- Fiber registry under GC stress.
;;;
;;; Regression test for the `entry_arg` GC hazard fixed 2026-04-22:
;;; make-fiber used to stash the tagged Lisp address of the fiber
;;; wrapper in the C struct's entry_arg field, which is a raw void*
;;; invisible to GC.  Under load, a copying GC moved the wrapper;
;;; the next fiber's trampoline dereferenced the stale address.
;;; Surfaced in contrib/sb-fiber/examples/banner-server.lisp around
;;; request 1300.  Fixed by keying the Lisp wrapper through
;;; *FIBER-REGISTRY* on the C SAP (which doesn't move).  This test
;;; runs through the create/first-switch/exit/destroy cycle many
;;; times with aggressive GCs interleaved, so if the wrapper's
;;; address gets invalidated between create and first switch we
;;; will notice.

(with-test (:name (:fiber :registry-survives-gc-churn))
  (let* ((main (make-main-fiber))
         (n-iter 5000)
         (hits 0))
    (dotimes (i n-iter)
      (let ((expected i))
        (let ((child (make-fiber
                      (lambda ()
                        (incf hits)
                        (assert (= expected i))
                        (fiber-switch sb-fiber::*current-fiber* main)))))
          (fiber-switch main child)
          (destroy-fiber child))
        (when (zerop (mod i 250)) (sb-ext:gc :full t))))
    (destroy-fiber main)
    (assert (= hits n-iter) () "expected ~D entries, got ~D" n-iter hits)))

;;; --- Cross-thread destroy.
;;;
;;; The fiber's owning thread created it; another thread destroys
;;; it.  sb_fiber_destroy's pool fast-path is only eligible when
;;; the caller is the fiber's owner, so this should fall through to
;;; the real munmap path.  Tests that path doesn't corrupt the
;;; origin thread's freelist or leak the registry entry.

#+sb-thread
(with-test (:name (:fiber :cross-thread-destroy))
  (let* ((created-fiber nil)
         (creator-done (sb-thread:make-semaphore))
         (destroyer-ok (sb-thread:make-semaphore))
         (t1 (sb-thread:make-thread
              (lambda ()
                (let ((mf (make-main-fiber)))
                  (setf created-fiber (make-fiber (lambda ())))
                  (sb-thread:signal-semaphore creator-done)
                  ;; Hold main fiber; don't exit the thread yet.
                  (sb-thread:wait-on-semaphore destroyer-ok :timeout 5)
                  (destroy-fiber mf)))
              :name "creator")))
    (sb-thread:wait-on-semaphore creator-done :timeout 5)
    ;; We (the main test thread) destroy a fiber owned by T1.
    (destroy-fiber created-fiber)
    (sb-thread:signal-semaphore destroyer-ok)
    (sb-thread:join-thread t1)))

;;; --- Signal safety across fiber-switch.
;;; A timer firing mid-switch invoking GC used to crash the collector
;;; (it would see FROM's binding_stack_pointer paired with TO's
;;; binding_stack_start, walking cross-region).  sb_fiber_switch now
;;; blocks deferrable signals across the critical window.

(with-test (:name (:fiber :signal-safe-under-timer-gc))
  (let* ((main (make-main-fiber))
         (fired 0)
         (child (make-fiber
                 (lambda ()
                   (loop (fiber-switch sb-fiber::*current-fiber* main))))))
    (let* ((tick
             (lambda ()
               (incf fired)
               ;; Handler does stack alloc + periodic GC so it exercises
               ;; the GC path from the signal-handler context.
               (let ((v (make-array 64 :initial-element fired)))
                 (declare (dynamic-extent v))
                 (dotimes (i 64) (setf (aref v i) (+ (aref v i) 1))))
               (when (zerop (mod fired 50)) (sb-ext:gc))))
           (timer (sb-ext:make-timer tick
                                     :thread sb-thread:*current-thread*)))
      (sb-ext:schedule-timer timer 0.0005 :repeat-interval 0.0005)
      (unwind-protect
           (dotimes (i 200000)
             (fiber-switch main child))
        (sb-ext:unschedule-timer timer)))
    ;; Test passes iff we didn't crash.  Timer cadence varies wildly
    ;; across OS schedulers -- linux fires hundreds per run, macOS
    ;; sometimes only a handful -- so we only require one fire to
    ;; confirm the signal path was actually exercised at least once.
    (assert (>= fired 1) () "timer never fired")
    (destroy-fiber child)
    (destroy-fiber main)))

;;; --- Park / unpark -------------------------------------------------

(with-test (:name (:fiber :park-unpark-basic))
  ;; Round-trip: child parks, main observes "parked", unparks, child
  ;; resumes from fiber-park and finishes.
  (let* ((main (make-main-fiber))
         (resumed 0)
         (child (make-fiber
                 (lambda ()
                   (fiber-park main)
                   (incf resumed)))))
    (fiber-switch main child)                         ; child runs, parks
    (assert (eq t (fiber-unpark child)))              ; was parked: T
    (assert (zerop resumed))
    (fiber-switch main child)                         ; resume past park
    (assert (= resumed 1))
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :unpark-before-park-credits))
  ;; Unpark a fiber that hasn't parked yet.  fiber-unpark returns NIL
  ;; (credit stashed, not parked).  The first fiber-park inside the
  ;; child then consumes the credit and returns WITHOUT switching; the
  ;; child proceeds and finishes normally.
  (let* ((main (make-main-fiber))
         (resumed 0)
         (child (make-fiber
                 (lambda ()
                   (fiber-park main)                   ; consumes credit
                   (incf resumed)))))
    (assert (null (fiber-unpark child)))              ; credit, not parked
    (fiber-switch main child)                         ; child runs to exit
    (assert (= resumed 1))
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :multiple-unparks-coalesce)
                  :skipped-on :win32)
  ;; Two unparks before any park: idempotent.  First park consumes the
  ;; single credit, second park actually suspends.
  (let* ((main (make-main-fiber))
         (parks 0)
         (child (make-fiber
                 (lambda ()
                   (fiber-park main) (incf parks)      ; consumes credit
                   (fiber-park main) (incf parks)))))  ; suspends
    (assert (null (fiber-unpark child)))
    (assert (null (fiber-unpark child)))              ; coalesces
    (fiber-switch main child)                         ; child runs, hits
                                                      ; second park, suspends
    (assert (= parks 1))                              ; first park no-op'd
    (assert (eq t (fiber-unpark child)))              ; now parked
    (fiber-switch main child)                         ; resume
    (assert (= parks 2))
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :unpark-signal-handler))
  ;; A signal handler on the same thread calls fiber-unpark on the
  ;; currently-parked fiber.  This is the race the PENDING state exists
  ;; to solve: if a timer fires between fiber-park's state flip and the
  ;; actual fiber-switch, the PARKED->READY transition in the handler
  ;; must be safe and the subsequent switch must not re-park.
  (let* ((main (make-main-fiber))
         (woken 0)
         (child (make-fiber
                 (lambda ()
                   (dotimes (i 100)
                     (fiber-park main)
                     (incf woken))))))
    (let* ((tick (lambda () (fiber-unpark child)))
           (timer (sb-ext:make-timer
                   tick :thread sb-thread:*current-thread*)))
      (sb-ext:schedule-timer timer 0.001 :repeat-interval 0.001)
      (unwind-protect
           (dotimes (i 100)
             ;; If child is already runnable (timer-unpark landed),
             ;; switch; otherwise fiber-unpark will wake it and the
             ;; next iteration will pick it up.
             (fiber-unpark child)
             (fiber-switch main child))
        (sb-ext:unschedule-timer timer)))
    (assert (= woken 100))
    (destroy-fiber child)
    (destroy-fiber main)))
