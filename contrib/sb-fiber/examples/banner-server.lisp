;;;; banner-server.lisp -- example sb-fiber usage
;;;;
;;;; A tiny HTTP/1.0 server that replies, for every request, with a
;;;; text/plain banner containing a random word from an embedded
;;;; dictionary (drawn in an ASCII block font), the current time of
;;;; day, and a monotonic connection counter.  One fiber per
;;;; connection, one OS thread running a cooperative scheduler, non-
;;;; blocking I/O via poll(2).  Demonstrates
;;;;
;;;;   * per-fiber state (the word, conn id, read buffer) that
;;;;     survives I/O yields unchanged,
;;;;   * cooperative yield-on-EAGAIN wrappers over raw fds,
;;;;   * a userspace scheduler with a runnable queue and a poll-
;;;;     backed wait queue -- ~80 lines of glue on top of the
;;;;     primitive.
;;;;
;;;; Linux/*BSD (poll).  Usage:
;;;;   sbcl --load banner-server.lisp --eval "(sb-fiber-banner:run :port 8080)"
;;;;   # in another shell:
;;;;   curl localhost:8080
;;;;   wrk -t1 -c200 -d10s http://localhost:8080/

(require :sb-fiber)
(require :sb-bsd-sockets)
(require :sb-posix)

(defpackage :sb-fiber-banner
  (:use :cl :sb-fiber)
  (:export #:run))

(in-package :sb-fiber-banner)

;;; --- Embedded dictionary ---------------------------------------

(defparameter *words*
  '("abeyance" "bauxite" "cognizant" "diaphanous" "effervescent"
    "forthright" "gregarious" "halcyon" "incandescent" "juxtapose"
    "kaleidoscope" "labyrinthine" "mercurial" "nonchalant" "obstinate"
    "pachyderm" "quintessence" "recalcitrant" "sycophant" "tantamount"
    "ubiquitous" "vicissitude" "whimsical" "xenophilia" "yonder"
    "zephyr" "akimbo" "brogue" "cantankerous" "dulcet"
    "ebullient" "filigree" "garrulous" "heliotrope" "ingenue"
    "jollity" "kismet" "lugubrious" "mellifluous" "nebulous"
    "oscillate" "penumbra" "quixotic" "resplendent" "serendipity"
    "truculent" "untoward" "vagabond" "wanderlust" "xylophone"))

;;; --- 5x5 block-letter font for the word headline ---------------
;;;
;;; Each letter is 5 strings of 5 columns; '#' = ink, '.' = blank.
;;; Looked up by uppercased character; unknowns become spaces.

(defparameter *font*
  '((#\A "..#.." ".#.#." "#####" "#...#" "#...#")
    (#\B "####." "#...#" "####." "#...#" "####.")
    (#\C ".####" "#...." "#...." "#...." ".####")
    (#\D "####." "#...#" "#...#" "#...#" "####.")
    (#\E "#####" "#...." "####." "#...." "#####")
    (#\F "#####" "#...." "####." "#...." "#....")
    (#\G ".####" "#...." "#..##" "#...#" ".####")
    (#\H "#...#" "#...#" "#####" "#...#" "#...#")
    (#\I "#####" "..#.." "..#.." "..#.." "#####")
    (#\J "####." "...#." "...#." "#..#." ".##..")
    (#\K "#...#" "#..#." "###.." "#..#." "#...#")
    (#\L "#...." "#...." "#...." "#...." "#####")
    (#\M "#...#" "##.##" "#.#.#" "#...#" "#...#")
    (#\N "#...#" "##..#" "#.#.#" "#..##" "#...#")
    (#\O ".###." "#...#" "#...#" "#...#" ".###.")
    (#\P "####." "#...#" "####." "#...." "#....")
    (#\Q ".###." "#...#" "#.#.#" "#..#." ".##.#")
    (#\R "####." "#...#" "####." "#..#." "#...#")
    (#\S ".####" "#...." ".###." "....#" "####.")
    (#\T "#####" "..#.." "..#.." "..#.." "..#..")
    (#\U "#...#" "#...#" "#...#" "#...#" ".###.")
    (#\V "#...#" "#...#" "#...#" ".#.#." "..#..")
    (#\W "#...#" "#...#" "#.#.#" "##.##" "#...#")
    (#\X "#...#" ".#.#." "..#.." ".#.#." "#...#")
    (#\Y "#...#" ".#.#." "..#.." "..#.." "..#..")
    (#\Z "#####" "....#" "..#.." "#...." "#####")))

(defun word-art (word)
  "Return a list of 5 strings rendering WORD in big letters."
  (let ((rows (list "" "" "" "" "")))
    (loop for raw-ch across word
          for ch = (char-upcase raw-ch)
          for row = (assoc ch *font* :test #'char=)
          for glyph = (or (cdr row)
                          (list "....." "....." "....." "....." "....."))
          do (setf rows
                   (loop for r in rows
                         for g in glyph
                         collect (concatenate 'string r g " "))))
    ;; . -> space, # -> full-block.
    (mapcar (lambda (s)
              (map 'string
                   (lambda (c) (case c (#\# #\█) (#\. #\space) (t c)))
                   s))
            rows)))

;;; --- Banner assembly -------------------------------------------

(defun pick-word () (nth (random (length *words*)) *words*))

(defun now-hhmmss ()
  (multiple-value-bind (sec min hr) (get-decoded-time)
    (format nil "~2,'0D:~2,'0D:~2,'0D" hr min sec)))

(defun render-banner (word time count)
  (let* ((art (word-art word))
         (width (max 40 (+ 4 (reduce #'max art :key #'length))))
         (hline (make-string width :initial-element #\─)))
    (with-output-to-string (s)
      (format s "┌~A┐~%" hline)
      (dolist (row art)
        (format s "│ ~vA │~%" (- width 2) row))
      (format s "├~A┤~%" hline)
      (format s "│ ~vA │~%" (- width 2) (format nil "word:       ~A" word))
      (format s "│ ~vA │~%" (- width 2) (format nil "time:       ~A" time))
      (format s "│ ~vA │~%" (- width 2) (format nil "connection: #~D" count))
      (format s "└~A┘~%" hline))))

;;; --- Raw syscalls ----------------------------------------------
;;;
;;; We call accept/read/write directly so that EAGAIN returns a
;;; value we can test, rather than being translated into a Lisp
;;; condition by sb-bsd-sockets.

(sb-alien:define-alien-routine "accept" sb-alien:int
  (fd sb-alien:int) (addr (* t)) (addrlen (* t)))

(sb-alien:define-alien-routine ("read" raw-read) sb-alien:long
  (fd sb-alien:int) (buf (* t)) (count sb-alien:unsigned-long))

(sb-alien:define-alien-routine ("write" raw-write) sb-alien:long
  (fd sb-alien:int) (buf (* t)) (count sb-alien:unsigned-long))

(sb-alien:define-alien-routine "poll" sb-alien:int
  (fds (* t)) (nfds sb-alien:unsigned-long) (timeout sb-alien:int))

(defconstant +pollin+  #x0001)
(defconstant +pollout+ #x0004)
(defconstant +pollfd-size+ 8)  ; {int fd; short events; short revents;}

(defun set-nonblocking (fd)
  (let ((flags (sb-posix:fcntl fd sb-posix:f-getfl)))
    (sb-posix:fcntl fd sb-posix:f-setfl (logior flags sb-posix:o-nonblock))))

(defun errno-would-block-p ()
  (let ((e (sb-alien:get-errno)))
    (or (= e sb-posix:eagain) (= e sb-posix:ewouldblock))))

;;; --- Scheduler -------------------------------------------------
;;;
;;; Two data structures, per-thread:
;;;   *RUNNABLE*  fifo of fibers ready to resume.
;;;   *WAITERS*   list of (fd events fiber) entries waiting on I/O.
;;;
;;; On yield, a fiber switches to *SCHEDULER*; the scheduler runs
;;; everything currently runnable, then poll()s on the waiters.
;;; No per-fd registration state -- poll() rebuilds its view from
;;; *WAITERS* on each iteration.

(defvar *scheduler*)
(defvar *runnable*)
(defvar *waiters*)
(defvar *connection-count* 0)

(defun schedule! (fiber)
  (setf *runnable* (nconc *runnable* (list fiber))))

(defun yield-for-io (fd events)
  "Park current fiber on FD / EVENTS; yield to scheduler."
  (push (list fd events (current-fiber)) *waiters*)
  (fiber-switch (current-fiber) *scheduler*))

(defun yield-until-readable (fd) (yield-for-io fd +pollin+))
(defun yield-until-writable (fd) (yield-for-io fd +pollout+))

(defun spawn (fn)
  (let ((f (make-fiber
            (lambda ()
              (handler-case (funcall fn)
                (error (e)
                  (format *error-output* "~&fiber error: ~A~%" e)
                  (finish-output *error-output*)))))))
    ;; Have the fiber auto-return to the scheduler when its thunk
    ;; finishes -- otherwise fiber_trampoline_c would trap.
    (setf (sb-fiber::%fiber-return-fiber-at (sb-fiber::fiber-sap f))
          (sb-fiber::fiber-sap *scheduler*))
    (schedule! f)))

(defun scheduler-loop ()
  (sb-alien:with-alien ((pfds (sb-alien:array sb-alien:unsigned-char 8192)))
    (loop
      ;; Drain runnables.
      (loop while *runnable*
            do (let ((f (pop *runnable*)))
                 (fiber-switch *scheduler* f)))
      (unless *waiters*
        (return))
      ;; Build pollfd array.
      (let ((n (min (length *waiters*)
                    (floor 8192 +pollfd-size+))))
        (loop for i from 0 below n
              for (fd events nil) in *waiters*
              for off = (* i +pollfd-size+)
              do (setf (sb-sys:sap-ref-32 (sb-alien:alien-sap pfds) off) fd)
                 (setf (sb-sys:sap-ref-16 (sb-alien:alien-sap pfds)
                                          (+ off 4)) events)
                 (setf (sb-sys:sap-ref-16 (sb-alien:alien-sap pfds)
                                          (+ off 6)) 0))
        (let ((rc (poll (sb-alien:alien-sap pfds) n -1)))
          (when (minusp rc) (error "poll failed"))
          ;; Walk revents; mark ready waiters runnable, remove from list.
          (let ((new-waiters '()))
            (loop for (fd events fiber) in *waiters*
                  for i from 0
                  for off = (* i +pollfd-size+)
                  for revents = (if (< i n)
                                    (sb-sys:sap-ref-16
                                     (sb-alien:alien-sap pfds) (+ off 6))
                                    0)
                  do (if (and (< i n) (plusp (logand revents events)))
                         (schedule! fiber)
                         (push (list fd events fiber) new-waiters)))
            (setf *waiters* (nreverse new-waiters))))))))

;;; --- I/O wrappers ----------------------------------------------

(defun nb-accept (listen-fd)
  (loop
    (let ((fd (accept listen-fd nil nil)))
      (cond ((not (minusp fd))
             (set-nonblocking fd)
             (return fd))
            ((errno-would-block-p)
             (yield-until-readable listen-fd))
            (t
             (error "accept failed, errno=~D" (sb-alien:get-errno)))))))

(defun nb-read (fd buffer)
  "Read up to (length BUFFER) bytes into BUFFER.  Returns count,
0 for EOF, yields on EAGAIN."
  (loop
    (let ((n (sb-sys:with-pinned-objects (buffer)
               (raw-read fd (sb-sys:vector-sap buffer) (length buffer)))))
      (cond ((not (minusp n)) (return n))
            ((errno-would-block-p) (yield-until-readable fd))
            (t (error "read failed, errno=~D" (sb-alien:get-errno)))))))

(defun nb-write-all (fd bytes)
  (let ((len (length bytes)) (off 0))
    (loop while (< off len) do
      (let ((n (sb-sys:with-pinned-objects (bytes)
                 (raw-write fd (sb-sys:sap+ (sb-sys:vector-sap bytes) off)
                            (- len off)))))
        (cond ((not (minusp n)) (incf off n))
              ((errno-would-block-p) (yield-until-writable fd))
              (t (error "write failed, errno=~D"
                        (sb-alien:get-errno))))))))

;;; --- HTTP handler ----------------------------------------------

(defun handle-connection (client-fd)
  (let ((conn-id (incf *connection-count*))
        (buf (make-array 2048 :element-type '(unsigned-byte 8))))
    (unwind-protect
         (progn
           ;; Read a single chunk -- clients we care about (curl, wrk,
           ;; ab) send the whole request in one packet.  A production
           ;; server would loop until it saw CRLF CRLF.
           (nb-read client-fd buf)
           (let* ((word (pick-word))
                  (body (render-banner word (now-hhmmss) conn-id))
                  (body-bytes (sb-ext:string-to-octets
                               body :external-format :utf-8))
                  (head (format nil
                                "HTTP/1.0 200 OK~C~C~
                                 Content-Type: text/plain; charset=utf-8~C~C~
                                 Content-Length: ~D~C~C~
                                 Connection: close~C~C~C~C"
                                #\Return #\Newline
                                #\Return #\Newline
                                (length body-bytes)
                                #\Return #\Newline
                                #\Return #\Newline
                                #\Return #\Newline))
                  (head-bytes (sb-ext:string-to-octets
                               head :external-format :utf-8))
                  (all-bytes (concatenate '(vector (unsigned-byte 8))
                                          head-bytes body-bytes)))
             (nb-write-all client-fd all-bytes)))
      (sb-posix:close client-fd))))

;;; --- Listener --------------------------------------------------

;;; The listener socket object must be kept alive -- sb-bsd-sockets
;;; attaches a finalizer that closes the fd when the Lisp wrapper is
;;; GC'd.  Storing it in a defvar pins it for the life of the process.
(defvar *listener-socket* nil)

(defun make-listener (port)
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket
                             :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
    (sb-bsd-sockets:socket-bind sock #(0 0 0 0) port)
    (sb-bsd-sockets:socket-listen sock 1024)
    (setf *listener-socket* sock)
    (let ((fd (sb-bsd-sockets:socket-file-descriptor sock)))
      (set-nonblocking fd)
      fd)))

(defun acceptor-body (listen-fd)
  (loop
    (let ((client-fd (nb-accept listen-fd)))
      (spawn (lambda () (handle-connection client-fd))))))

(defun run (&key (port 8000))
  "Start the banner server.  Blocks until *WAITERS* and *RUNNABLE*
are both empty (never, in practice -- the acceptor loops forever)."
  (let ((listen-fd (make-listener port)))
    (format *error-output* "~&banner-server listening on :~D~%" port)
    (finish-output *error-output*)
    (let ((*scheduler* (make-main-fiber))
          (*runnable* '())
          (*waiters* '()))
      (spawn (lambda () (acceptor-body listen-fd)))
      (scheduler-loop))))
