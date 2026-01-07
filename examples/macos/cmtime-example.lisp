;;;; cmtime-example.lisp - Core Media CMTime Arithmetic Demo
;;;;
;;;; Demonstrates using Apple's Core Media framework for video/audio timing.
;;;; CMTime is a 24-byte struct representing rational time (value/timescale).
;;;; CMTimeRange is a 48-byte struct (start time + duration).
;;;;
;;;; Requires: macOS with CoreMedia framework
;;;; Uses: struct-by-value returns (srbv branch feature)

(defpackage #:cmtime-example
  (:use #:cl)
  (:export #:run-demo))

(in-package #:cmtime-example)

;;; ============================================================================
;;; CMTime Structure Definition
;;; ============================================================================

;;; CMTime is 24 bytes:
;;;   int64_t value      (8 bytes) - time value
;;;   int32_t timescale  (4 bytes) - units per second
;;;   uint32_t flags     (4 bytes) - validity flags
;;;   int64_t epoch      (8 bytes) - timeline epoch
;;;
;;; This is returned via hidden pointer on both x86-64 and ARM64
;;; (exceeds 16-byte register return limit)

(define-alien-type cmtime
    (struct cmtime
      (value (signed 64))
      (timescale (signed 32))
      (flags (unsigned 32))
      (epoch (signed 64))))

;;; CMTimeRange is 48 bytes (two CMTime structs)
(define-alien-type cmtime-range
    (struct cmtime-range
      (start cmtime)
      (duration cmtime)))

;;; CMTimeMapping is 96 bytes (two CMTimeRange structs)
(define-alien-type cmtime-mapping
    (struct cmtime-mapping
      (source cmtime-range)
      (target cmtime-range)))

;;; ============================================================================
;;; CMTime Flag Constants
;;; ============================================================================

(defconstant +cmtime-flags-valid+ #x01)
(defconstant +cmtime-flags-has-been-rounded+ #x02)
(defconstant +cmtime-flags-positive-infinity+ #x04)
(defconstant +cmtime-flags-negative-infinity+ #x08)
(defconstant +cmtime-flags-indefinite+ #x10)
(defconstant +cmtime-flags-implied-value-fraction-numerator+ #x20)

;;; ============================================================================
;;; Load Core Media Framework
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (load-shared-object "/System/Library/Frameworks/CoreMedia.framework/CoreMedia")
    (error (c)
      (warn "Could not load CoreMedia framework: ~A~%This demo requires macOS." c))))

;;; ============================================================================
;;; CMTime Functions (all return 24-byte struct via hidden pointer)
;;; ============================================================================

(define-alien-routine ("CMTimeMake" cmtime-make) cmtime
  (value (signed 64))
  (timescale (signed 32)))

(define-alien-routine ("CMTimeMakeWithSeconds" cmtime-make-with-seconds) cmtime
  (seconds double)
  (preferred-timescale (signed 32)))

(define-alien-routine ("CMTimeAdd" cmtime-add) cmtime
  (lhs cmtime)
  (rhs cmtime))

(define-alien-routine ("CMTimeSubtract" cmtime-subtract) cmtime
  (lhs cmtime)
  (rhs cmtime))

(define-alien-routine ("CMTimeMultiply" cmtime-multiply) cmtime
  (time cmtime)
  (multiplier (signed 32)))

(define-alien-routine ("CMTimeMultiplyByFloat64" cmtime-multiply-by-float64) cmtime
  (time cmtime)
  (multiplier double))

(define-alien-routine ("CMTimeConvertScale" cmtime-convert-scale) cmtime
  (time cmtime)
  (new-timescale (signed 32))
  (rounding-method (unsigned 32)))

(define-alien-routine ("CMTimeGetSeconds" cmtime-get-seconds) double
  (time cmtime))

(define-alien-routine ("CMTimeCompare" cmtime-compare) (signed 32)
  (time1 cmtime)
  (time2 cmtime))

(define-alien-routine ("CMTimeMinimum" cmtime-minimum) cmtime
  (time1 cmtime)
  (time2 cmtime))

(define-alien-routine ("CMTimeMaximum" cmtime-maximum) cmtime
  (time1 cmtime)
  (time2 cmtime))

(define-alien-routine ("CMTimeAbsoluteValue" cmtime-absolute-value) cmtime
  (time cmtime))

;;; ============================================================================
;;; CMTimeRange Functions (48-byte struct returns)
;;; ============================================================================

(define-alien-routine ("CMTimeRangeMake" cmtime-range-make) cmtime-range
  (start cmtime)
  (duration cmtime))

(define-alien-routine ("CMTimeRangeGetUnion" cmtime-range-get-union) cmtime-range
  (range1 cmtime-range)
  (range2 cmtime-range))

(define-alien-routine ("CMTimeRangeGetIntersection" cmtime-range-get-intersection) cmtime-range
  (range1 cmtime-range)
  (range2 cmtime-range))

(define-alien-routine ("CMTimeRangeGetEnd" cmtime-range-get-end) cmtime
  (range cmtime-range))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun cmtime-valid-p (time)
  "Check if a CMTime is valid."
  (not (zerop (logand (slot time 'flags) +cmtime-flags-valid+))))

(defun print-cmtime (time &optional (stream *standard-output*))
  "Print a CMTime in a readable format."
  (cond
    ((not (cmtime-valid-p time))
     (format stream "CMTime(invalid)"))
    ((not (zerop (logand (slot time 'flags) +cmtime-flags-positive-infinity+)))
     (format stream "CMTime(+∞)"))
    ((not (zerop (logand (slot time 'flags) +cmtime-flags-negative-infinity+)))
     (format stream "CMTime(-∞)"))
    ((not (zerop (logand (slot time 'flags) +cmtime-flags-indefinite+)))
     (format stream "CMTime(indefinite)"))
    (t
     (format stream "CMTime(~D/~D = ~,4Fs)"
             (slot time 'value)
             (slot time 'timescale)
             (cmtime-get-seconds time)))))

(defun print-cmtime-range (range &optional (stream *standard-output*))
  "Print a CMTimeRange."
  (format stream "CMTimeRange[")
  (print-cmtime (slot range 'start) stream)
  (format stream " + ")
  (print-cmtime (slot range 'duration) stream)
  (format stream "]"))

;;; ============================================================================
;;; Video Frame Rate Examples
;;; ============================================================================

(defun demo-video-frame-rates ()
  "Demonstrate CMTime usage for common video frame rates."
  (format t "~%=== Video Frame Rate Representations ===~%~%")

  ;; Common frame rates
  (let ((rates '((24 . "Film (24 fps)")
                 (25 . "PAL (25 fps)")
                 (30 . "NTSC (30 fps)")
                 (60 . "High frame rate (60 fps)")
                 (120 . "Slow-mo (120 fps)"))))
    (dolist (rate-info rates)
      (let* ((fps (car rate-info))
             (name (cdr rate-info))
             (frame-duration (cmtime-make 1 fps)))
        (format t "~A:~%" name)
        (format t "  One frame = ")
        (print-cmtime frame-duration)
        (format t "~%")

        ;; Calculate duration of 1 second of video
        (let ((one-sec (cmtime-multiply frame-duration fps)))
          (format t "  ~D frames = " fps)
          (print-cmtime one-sec)
          (format t "~%~%")))))

  ;; NTSC drop-frame (29.97 fps = 30000/1001)
  (format t "NTSC Drop-frame (29.97 fps):~%")
  (let ((ntsc-frame (cmtime-make 1001 30000)))
    (format t "  One frame = ")
    (print-cmtime ntsc-frame)
    (format t "~%")

    ;; Duration of 1 minute of video (1798 frames for 29.97 drop-frame)
    (let ((one-minute (cmtime-multiply ntsc-frame 1798)))
      (format t "  1798 frames ≈ ")
      (print-cmtime one-minute)
      (format t " (approximately 1 minute)~%~%"))))

(defun demo-time-arithmetic ()
  "Demonstrate CMTime arithmetic operations."
  (format t "~%=== CMTime Arithmetic ===~%~%")

  ;; Create some times
  (let ((t1 (cmtime-make-with-seconds 10.5d0 600))   ; 10.5 seconds
        (t2 (cmtime-make-with-seconds 5.25d0 600)))  ; 5.25 seconds

    (format t "Time 1: ")
    (print-cmtime t1)
    (format t "~%")

    (format t "Time 2: ")
    (print-cmtime t2)
    (format t "~%~%")

    ;; Addition
    (let ((sum (cmtime-add t1 t2)))
      (format t "t1 + t2 = ")
      (print-cmtime sum)
      (format t "~%"))

    ;; Subtraction
    (let ((diff (cmtime-subtract t1 t2)))
      (format t "t1 - t2 = ")
      (print-cmtime diff)
      (format t "~%"))

    ;; Multiplication
    (let ((scaled (cmtime-multiply-by-float64 t1 2.0d0)))
      (format t "t1 * 2 = ")
      (print-cmtime scaled)
      (format t "~%"))

    ;; Comparison
    (let ((cmp (cmtime-compare t1 t2)))
      (format t "~%Compare t1 to t2: ~A~%"
              (cond ((> cmp 0) "t1 > t2")
                    ((< cmp 0) "t1 < t2")
                    (t "t1 = t2"))))

    ;; Min/Max
    (let ((min-t (cmtime-minimum t1 t2))
          (max-t (cmtime-maximum t1 t2)))
      (format t "Minimum: ")
      (print-cmtime min-t)
      (format t "~%")
      (format t "Maximum: ")
      (print-cmtime max-t)
      (format t "~%"))))

(defun demo-time-ranges ()
  "Demonstrate CMTimeRange operations."
  (format t "~%=== CMTimeRange Operations ===~%~%")

  ;; Create time ranges representing video segments
  (let* ((start1 (cmtime-make-with-seconds 0.0d0 600))
         (dur1 (cmtime-make-with-seconds 10.0d0 600))
         (range1 (cmtime-range-make start1 dur1))

         (start2 (cmtime-make-with-seconds 5.0d0 600))
         (dur2 (cmtime-make-with-seconds 10.0d0 600))
         (range2 (cmtime-range-make start2 dur2)))

    (format t "Range 1: ")
    (print-cmtime-range range1)
    (format t "~%")

    (format t "Range 2: ")
    (print-cmtime-range range2)
    (format t "~%~%")

    ;; Get end times
    (let ((end1 (cmtime-range-get-end range1))
          (end2 (cmtime-range-get-end range2)))
      (format t "End of Range 1: ")
      (print-cmtime end1)
      (format t "~%")
      (format t "End of Range 2: ")
      (print-cmtime end2)
      (format t "~%~%"))

    ;; Union
    (let ((union (cmtime-range-get-union range1 range2)))
      (format t "Union: ")
      (print-cmtime-range union)
      (format t "~%"))

    ;; Intersection
    (let ((intersection (cmtime-range-get-intersection range1 range2)))
      (format t "Intersection: ")
      (print-cmtime-range intersection)
      (format t "~%"))))

(defun demo-timescale-conversion ()
  "Demonstrate timescale conversion for editing workflows."
  (format t "~%=== Timescale Conversion ===~%~%")

  ;; Video editors often work with different timescales
  ;; Common ones: 600 (divisible by 24, 25, 30), 90000 (MPEG), 48000 (audio)

  (let ((time-600 (cmtime-make 3000 600)))  ; 5 seconds at 600 scale
    (format t "Original (timescale 600): ")
    (print-cmtime time-600)
    (format t "~%~%")

    ;; Convert to different timescales
    ;; Rounding method 0 = kCMTimeRoundingMethod_RoundHalfAwayFromZero
    (dolist (scale '(24 30 90000 48000))
      (let ((converted (cmtime-convert-scale time-600 scale 0)))
        (format t "  Converted to ~D: " scale)
        (print-cmtime converted)
        (format t "~%")))))

;;; ============================================================================
;;; Main Demo
;;; ============================================================================

(defun run-demo ()
  "Run the complete CMTime demo."
  (format t "~%Core Media CMTime Demo~%")
  (format t "======================~%")
  (format t "~%CMTime is a 24-byte struct used throughout Apple's AV frameworks~%")
  (format t "for precise time representation. CMTimeRange (48 bytes) represents~%")
  (format t "time intervals. Both are returned by value via hidden pointer.~%")

  (demo-video-frame-rates)
  (demo-time-arithmetic)
  (demo-time-ranges)
  (demo-timescale-conversion)

  (format t "~%Demo complete.~%"))

;;; ============================================================================
;;; Notes on ABI behavior
;;; ============================================================================
#|
CMTime (24 bytes) and CMTimeRange (48 bytes) return conventions:

On both x86-64 and ARM64:
- CMTime (24 bytes): Exceeds 16-byte limit -> hidden pointer return
  - x86-64: Hidden pointer in RDI, returned in RAX
  - ARM64: Hidden pointer in x8

- CMTimeRange (48 bytes): Definitely hidden pointer return
  - Same convention as CMTime

The CMTime functions are interesting because they perform exact rational
arithmetic (value/timescale) rather than floating-point, avoiding
rounding errors in video editing calculations.

Typical video timescales:
- 600: Divisible by 24, 25, 30 (common frame rates)
- 30000: For 29.97 fps (30000/1001) NTSC
- 90000: MPEG transport stream clock
- 48000: Audio sample rate
|#
