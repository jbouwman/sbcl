# Saved Darwin callback pages must be touched after gaining execute permission.
# Exercise callback entry independently of Epsilon's scheduler. The reported
# first-fiber fault is intermittent, so start the saved image repeatedly.
. ./subr.sh

create_test_subdirectory
tmpcore=$TEST_DIRECTORY/$TEST_FILESTEM.core

run_sbcl <<EOF_LISP
  #-(and darwin arm64 sb-thread sb-fiber) (exit :code 2)
  (require :sb-fiber)
  (defun exercise-saved-fiber-callback ()
    (sb-ext:with-timeout 15
      (let ((threads
              (loop repeat 4 collect
                (sb-thread:make-thread
                  (lambda ()
                    (sb-fiber:with-fiber-thread ()
                      (dotimes (i 250)
                        (sb-fiber:with-fiber (fiber (lambda () 42))
                          (assert (= 42 (sb-fiber:resume-fiber fiber))))))
                    :done)))))
        (dolist (thread threads)
          (assert (eq :done (sb-thread:join-thread thread :timeout 10
                                                          :default :timeout))))))
    (sb-ext:exit :code $EXIT_LISP_WIN))
  (save-lisp-and-die "$tmpcore" :toplevel #'exercise-saved-fiber-callback)
EOF_LISP
status=$?
if [ "$status" -eq 2 ]; then
    exit $EXIT_TEST_WIN
fi
check_status_maybe_lose "saving fiber callback core" "$status" 0 "saved"

iteration=0
while [ "$iteration" -lt 40 ]; do
    run_sbcl_with_core "$tmpcore"
    check_status_maybe_lose "saved fiber callback run $iteration" "$?" \
        "$EXIT_LISP_WIN" "completed"
    iteration=$((iteration + 1))
done

exit $EXIT_TEST_WIN
