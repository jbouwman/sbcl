. ./subr.sh

use_test_subdirectory

tmpcore=$TEST_FILESTEM.core

# Save seeks over the all-zero blocks of each space instead of writing them.
# Garbage interleaved with data that stays live leaves free, zeroed pages
# inside the dynamic space when the collector does not move objects.
run_sbcl <<EOF
  (defvar *live*
    (loop for i below 256
          collect (progn (make-array 65536 :element-type '(unsigned-byte 8))
                         (make-array 16 :initial-element i))))
  (save-lisp-and-die "$tmpcore")
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit --noprint <<EOF
  (exit :code (if (equal (loop for v in *live* collect (aref v 15))
                         (loop for i below 256 collect i))
                  $EXIT_LISP_WIN
                  1))
EOF
check_status_maybe_lose "core saved with zero blocks" $?

# Where the filesystem keeps holes, the zero blocks must occupy no space.
cat > count-zero-blocks.lisp <<EOF
  (format t "~D~%"
          (funcall (compile nil '(lambda (path)
                                  (with-open-file (s path :element-type '(unsigned-byte 8))
                                    (let ((block (make-array 4096 :element-type '(unsigned-byte 8)))
                                          (zeros 0))
                                      (declare (fixnum zeros))
                                      (loop for n = (read-sequence block s)
                                            while (= n 4096)
                                            when (every #'zerop block)
                                              do (incf zeros 4096))
                                      zeros))))
                   "$tmpcore"))
EOF
zero_bytes=`run_sbcl < count-zero-blocks.lisp | tail -n 1`
length=`wc -c < "$tmpcore" | tr -d ' '`
allocated=$((`du -k "$tmpcore" | cut -f1` * 1024))
echo "core $length bytes, $zero_bytes in all-zero 4 KB blocks, $allocated allocated"

dd if=/dev/zero of=hole-probe bs=1 count=1 seek=4194304 2>/dev/null
if [ `du -k hole-probe | cut -f1` -lt 1024 ] && [ "$zero_bytes" -ge 4194304 ]; then
    if [ "$allocated" -gt $((length - zero_bytes / 2)) ]; then
        echo "test core zero blocks as holes failed"
        exit $EXIT_LOSE
    fi
    echo "test core zero blocks as holes ok"
fi

exit $EXIT_TEST_WIN
