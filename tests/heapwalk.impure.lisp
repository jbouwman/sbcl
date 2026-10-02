
(defun mul (x y) (declare (sb-vm:signed-word x y)) (* x y))
(compile 'mul)

(defun manymul (n &aux res)
  (dotimes (i n res)
    (setq res (mul (floor (- (expt 2 (- sb-vm:n-word-bits 2))) 1000)
                   (+ i 1000)))))
(compile 'manymul)

(defun walk ()
  (let ((v (make-array 1000))
        (ct 0))
    (sb-vm:map-allocated-objects
     (lambda (obj type size)
       (declare (ignore size))
       (when (and (= type sb-vm:list-pointer-lowtag)
                  (= (sb-kernel:generation-of obj) 0)
                  (< ct 1000))
         (setf (aref v ct) obj)
         (incf ct)))
     :dynamic)
    (let ((*print-level* 2)
          (*print-length* 4)
          (*standard-output* (make-broadcast-stream)))
      (dotimes (i ct)
        (princ (aref v i))))))
(compile 'walk)

;;; As a precondition to asserting that heap walking did not
;;; visit an alleged cons that is a filler object,
;;; assert that there is the telltale pattern (if applicable).
;;; x86-64 no longer leaves a stray 0xFF..FFF word in the heap.
;;; That bit pattern came from signed integer multiplication where the final result
;;; was a bignum having 1 payload word, but the intermediate result was a bignum
;;; whose trailing word was all 1s. Being a redundant copy of the sign bit from the
;;; prior word, the bignum gets shortened. Only arm64 overallocates the bignum now.
#+arm64
(let ((product (manymul 1)))
  (sb-sys:with-pinned-objects (product)
    (let ((word (sb-sys:sap-ref-word
                 (sb-sys:int-sap (sb-kernel:get-lisp-obj-address product))
                 (- (ash 2 sb-vm:word-shift) sb-vm:other-pointer-lowtag))))
      (assert (= word sb-ext:most-positive-word)))))

(manymul 100)

;;; Granted it's not a great idea to assume that anything in the heap
;;; can be printed, but this test was a fairly easy way to get
;;;  "Unhandled memory fault at #xFFFFFFFFFFFFFFF0."
;;; The should print approximately one cons (for GC epoch)
(with-test (:name :heapwalk-safety)
  (progn (gc :gen 1) (manymul 100) (walk)))

;;; Under mark-region, a small-object page's WORDS-USED counts its used
;;; lines rather than marking a high-water point, and lines allocated into
;;; since the last collection carry no allocation bits. The walk stopped
;;; short of objects behind a gap on a page and skipped objects on fresh
;;; lines, so code compiled after startup was missing from MAP-CODE-OBJECTS
;;; and from the reports SB-SPROF builds with it.
(with-test (:name (sb-vm:map-code-objects :code-compiled-after-startup))
  (flet ((compile-some (n)
           (let ((sb-c::*compile-to-memory-space* :dynamic))
             (loop for i below n
                   collect (compile nil `(lambda (x) (+ x ,i))))))
         (all-found-p (functions)
           (let ((codes (make-hash-table :test 'eq)))
             (sb-vm:map-code-objects (lambda (code) (setf (gethash code codes) t)))
             (every (lambda (f) (gethash (sb-kernel:fun-code-header f) codes))
                    functions))))
    (let ((functions '())
          (garbage (make-array 200)))
      ;; Interleave garbage with the code so that, once the garbage dies,
      ;; live code sits behind free lines on its pages.
      (dotimes (i 200)
        (setf (aref garbage i) (make-array 100))
        (setq functions (nconc (compile-some 1) functions)))
      (assert (all-found-p functions)) ; on fresh lines, before any collection
      (fill garbage nil)
      (gc :full t)
      (assert (all-found-p functions)) ; behind the gaps the collection left
      (setq functions (nconc (compile-some 50) functions))
      (assert (all-found-p functions)))))
