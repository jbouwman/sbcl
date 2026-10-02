# On Darwin JIT, the pages of the static code space that hold the alien
# callables saved in a core have to be touched after the space is made
# executable, as the Lisp code pages are, or the first instruction of a
# restored callable can fault although its mapping is executable. The
# fault is intermittent, so the saved core is started repeatedly.
. ./subr.sh

create_test_subdirectory
tmpcore=$TEST_DIRECTORY/$TEST_FILESTEM.core

run_sbcl <<EOF
  #-darwin-jit (exit :code 2)
  (define-alien-callable saved-callable int ((x int)) (* 2 x))
  (defun call-saved-callable ()
    (let ((callable (alien-callable-function 'saved-callable)))
      (dotimes (i 1000)
        (assert (= (alien-funcall callable i) (* 2 i)))))
    (exit :code $EXIT_LISP_WIN))
  (save-lisp-and-die "$tmpcore" :toplevel #'call-saved-callable)
EOF
status=$?
if [ "$status" -eq 2 ]; then
    exit $EXIT_TEST_WIN
fi
check_status_maybe_lose "saving a core with an alien callable" "$status" 0 "saved"

iteration=0
while [ "$iteration" -lt 20 ]; do
    run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit
    check_status_maybe_lose "restored callable, start $iteration" "$?" \
        "$EXIT_LISP_WIN" "called"
    iteration=$((iteration + 1))
done

exit $EXIT_TEST_WIN
