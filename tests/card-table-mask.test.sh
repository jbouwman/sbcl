# A core started with a larger or smaller dynamic space than it was saved
# with must mark, from its saved code, the card that this process's card
# table assigns to each store. On x86-64 and arm64 the barrier reads the
# mask from memory, so the table is also sized for this process's heap
# rather than for the heap the core was saved with.
. ./subr.sh

use_test_subdirectory

tmpcore=$TEST_FILESTEM.core

run_sbcl_with_args --dynamic-space-size 256MB --noinform --disable-debugger \
    --no-userinit --no-sysinit --noprint <<EOF
  (setq *features* (union *features* sb-impl:+internal-features+))
  #-(and 64-bit soft-card-marks) (exit :code 2)
  (defvar *saved-mask* (extern-alien "gc_card_table_mask" int))
  (defun store-into (vector index value)
    (declare (simple-vector vector) (fixnum index))
    (setf (svref vector index) value))
  (defun card (address)
    (ash address (- (integer-length (1- sb-vm:gencgc-card-bytes)))))
  (defun card-mark (address)
    (sb-sys:sap-ref-8 (extern-alien "gc_card_mark" sb-sys:system-area-pointer)
                      (logand (card address) (extern-alien "gc_card_table_mask" int))))
  (defun (setf card-mark) (value address)
    (setf (sb-sys:sap-ref-8 (extern-alien "gc_card_mark" sb-sys:system-area-pointer)
                            (logand (card address) (extern-alien "gc_card_table_mask" int)))
          value))
  (defun expected-mask ()
    ;; As compute_card_table_size: at least 2^13 cards, and a power of two
    ;; at least as large as the number of cards in dynamic space.
    (1- (ash 1 (max 13 (integer-length
                        (1- (ceiling (sb-ext:dynamic-space-size)
                                     sb-vm:gencgc-card-bytes)))))))
  (defun check-stores (megabytes)
    ;; Returns how many of the stores were to a card that the saved mask
    ;; would have confused with another one.
    (let* ((n-words (/ (* 4 1024 1024) sb-vm:n-word-bytes))
           (index (1- n-words))
           (mask (extern-alien "gc_card_table_mask" int))
           (vectors (loop repeat (/ megabytes 4)
                          collect (make-array n-words :initial-element 0)))
           (aliased 0))
      (dolist (vector vectors aliased)
        (let ((value (list vector)))
          (sb-sys:with-pinned-objects (vector)
            (let ((address (+ (sb-kernel:get-lisp-obj-address vector)
                              (- sb-vm:other-pointer-lowtag)
                              (ash (+ sb-vm:vector-data-offset index)
                                   sb-vm:word-shift))))
              (unless (= (logand (card address) mask)
                         (logand (card address) *saved-mask*))
                (incf aliased))
              (sb-sys:without-gcing
                ;; CARD_UNMARKED, then CARD_MARKED below
                (setf (card-mark address) #xff)
                (store-into vector index value)
                (assert (= (card-mark address) 0)))))))))
  (save-lisp-and-die "$tmpcore")
EOF
status=$?
if [ "$status" -eq 2 ]; then
    exit $EXIT_TEST_WIN
fi
check_status_maybe_lose "saving core at 256MB" "$status" 0 "saved"

run_sbcl_with_core "$tmpcore" --dynamic-space-size 1GB --noinform \
    --disable-debugger --no-userinit --no-sysinit --noprint <<EOF
  #+(or x86-64 arm64) (assert (= (extern-alien "gc_card_table_mask" int) (expected-mask)))
  (let ((aliased (check-stores 512)))
    (declare (ignorable aliased))
    ;; With the mask in memory the saving process sized its table for 256MB,
    ;; so some of these cards alias under the mask it had.
    #+(or x86-64 arm64) (assert (plusp aliased)))
  (gc :full t)
  (exit :code $EXIT_LISP_WIN)
EOF
check_status_maybe_lose "stores at a larger heap" "$?" "$EXIT_LISP_WIN" "marked"

run_sbcl_with_core "$tmpcore" --dynamic-space-size 128MB --noinform \
    --disable-debugger --no-userinit --no-sysinit --noprint <<EOF
  #+(or x86-64 arm64) (assert (= (extern-alien "gc_card_table_mask" int) (expected-mask)))
  (check-stores 32)
  (gc :full t)
  (exit :code $EXIT_LISP_WIN)
EOF
check_status_maybe_lose "stores at a smaller heap" "$?" "$EXIT_LISP_WIN" "marked"

exit $EXIT_TEST_WIN
