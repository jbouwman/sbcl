;;;; echo.lisp -- minimal sb-fiber example.
;;;;
;;;; Cooperative scheduler driven by poll(2), one fiber per TCP
;;;; connection.  Echoes the first read back to the client.  Linux/*BSD.
;;;;
;;;;   sbcl --load echo.lisp --eval "(sb-fiber-echo:run :port 8080)"
;;;;   curl -d hello localhost:8080

(require :sb-fiber)
(require :sb-bsd-sockets)
(require :sb-posix)

(defpackage :sb-fiber-echo (:use :cl :sb-fiber) (:export #:run))
(in-package :sb-fiber-echo)

;;; Raw syscalls -- we want EAGAIN as a value, not a condition.
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
(defconstant +pollfd-size+ 8) ; struct pollfd { int fd; short events,revents; }

(defun set-nonblocking (fd)
  (sb-posix:fcntl fd sb-posix:f-setfl
                  (logior (sb-posix:fcntl fd sb-posix:f-getfl)
                          sb-posix:o-nonblock)))

(defun would-block-p ()
  (let ((e (sb-alien:get-errno)))
    (or (= e sb-posix:eagain) (= e sb-posix:ewouldblock))))

;;; Scheduler: *RUNNABLE* fifo + *WAITERS* (fd events fiber) list.
;;; Yield switches to *SCHEDULER*; the scheduler drains runnables, then
;;; rebuilds a pollfd array from *WAITERS* and blocks in poll(2).
(defvar *scheduler*)
(defvar *runnable*)
(defvar *waiters*)

(defun yield-for-io (fd events)
  (push (list fd events (current-fiber)) *waiters*)
  (fiber-switch (current-fiber) *scheduler*))

;;; The first (fiber-switch *scheduler* f) sets f's return path back to
;;; *scheduler* implicitly, so a fresh fiber needs no extra wiring.
(defun spawn (fn)
  (setf *runnable*
        (nconc *runnable*
               (list (make-fiber
                      (lambda ()
                        (handler-case (funcall fn)
                          (error (e)
                            (format *error-output* "~&fiber: ~A~%" e))))))))
  nil)

(defun scheduler-loop ()
  (sb-alien:with-alien ((pfds (sb-alien:array sb-alien:unsigned-char 8192)))
    (loop
      (loop while *runnable* do (fiber-switch *scheduler* (pop *runnable*)))
      (unless *waiters* (return))
      (let* ((sap (sb-alien:alien-sap pfds))
             (n   (min (length *waiters*) (floor 8192 +pollfd-size+))))
        (loop for i below n
              for (fd events nil) in *waiters*
              for off = (* i +pollfd-size+)
              do (setf (sb-sys:sap-ref-32 sap off)       fd
                       (sb-sys:sap-ref-16 sap (+ off 4)) events
                       (sb-sys:sap-ref-16 sap (+ off 6)) 0))
        (when (minusp (poll sap n -1)) (error "poll failed"))
        (let (kept)
          (loop for w in *waiters*
                for i from 0
                for revents = (if (< i n)
                                  (sb-sys:sap-ref-16 sap (+ (* i +pollfd-size+) 6))
                                  0)
                do (if (and (< i n) (plusp (logand revents (second w))))
                       (setf *runnable* (nconc *runnable* (list (third w))))
                       (push w kept)))
          (setf *waiters* (nreverse kept)))))))

;;; I/O wrappers: yield on EAGAIN, retry on wake.
(defun nb-accept (lfd)
  (loop (let ((fd (accept lfd nil nil)))
          (cond ((not (minusp fd)) (set-nonblocking fd) (return fd))
                ((would-block-p)   (yield-for-io lfd +pollin+))
                (t (error "accept errno=~D" (sb-alien:get-errno)))))))

(defun nb-read (fd buf)
  (loop (let ((n (sb-sys:with-pinned-objects (buf)
                   (raw-read fd (sb-sys:vector-sap buf) (length buf)))))
          (cond ((not (minusp n)) (return n))
                ((would-block-p)  (yield-for-io fd +pollin+))
                (t (error "read errno=~D" (sb-alien:get-errno)))))))

(defun nb-write-all (fd bytes)
  (loop with len = (length bytes) and off = 0
        while (< off len)
        do (let ((n (sb-sys:with-pinned-objects (bytes)
                      (raw-write fd (sb-sys:sap+ (sb-sys:vector-sap bytes) off)
                                 (- len off)))))
             (cond ((not (minusp n)) (incf off n))
                   ((would-block-p)  (yield-for-io fd +pollout+))
                   (t (error "write errno=~D" (sb-alien:get-errno)))))))

;;; Echo handler.  BUF and N are per-fiber state; they survive any
;;; number of yields between the read and the write.
(defun handle (fd)
  (unwind-protect
       (let* ((buf (make-array 2048 :element-type '(unsigned-byte 8)))
              (n   (nb-read fd buf)))
         (when (plusp n) (nb-write-all fd (subseq buf 0 n))))
    (sb-posix:close fd)))

;;; sb-bsd-sockets finalizer would close the fd if we let the wrapper
;;; get GC'd; pin it for the life of the process.
(defvar *listener-socket* nil)

(defun make-listener (port)
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                          :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address s) t
          *listener-socket* s)
    (sb-bsd-sockets:socket-bind s #(0 0 0 0) port)
    (sb-bsd-sockets:socket-listen s 1024)
    (let ((fd (sb-bsd-sockets:socket-file-descriptor s)))
      (set-nonblocking fd)
      fd)))

(defun run (&key (port 8000))
  "Start the echo server on PORT.  Loops forever."
  (let ((lfd (make-listener port)))
    (format *error-output* "~&listening on :~D~%" port)
    (let ((*scheduler* (make-main-fiber)) (*runnable* '()) (*waiters* '()))
      (spawn (lambda ()
               (loop (let ((fd (nb-accept lfd)))
                       (spawn (lambda () (handle fd)))))))
      (scheduler-loop))))
