# A link core (SAVE-LISP-AND-DIE :LINK T) holds the pages that differ from
# the cores its process loaded, and refers to those cores for the rest.
# Save a chain of two, boot each, and check that a link core whose parent
# was replaced or removed does not load.
. ./subr.sh

use_test_subdirectory

first=$TEST_FILESTEM-first.core
second=$TEST_FILESTEM-second.core
other=$TEST_FILESTEM-other.core
monolithic=$TEST_FILESTEM-monolithic.core
rm -f "$first" "$second" "$other" "$monolithic"

run_sbcl <<EOF
  (setq *features* (union *features* sb-impl:+internal-features+))
  #-(and mark-region-gc (not immobile-space) (not win32)) (exit :code 2)
  (defvar *first* (loop for i below 100000 collect (format nil "first ~D" i)))
  (defun first-sum () (reduce #'+ *first* :key #'length))
  (save-lisp-and-die "$first" :link t)
EOF
status=$?
if [ "$status" -eq 2 ]; then
    exit $EXIT_TEST_WIN
fi
check_status_maybe_lose "link save over the base core" "$status" 0 "saved"

run_sbcl_with_core "$first" --noinform --disable-debugger \
    --no-userinit --no-sysinit --noprint <<EOF
  (assert (= (length *first*) 100000))
  (assert (string= (nth 99999 *first*) "first 99999"))
  (defvar *second* (make-hash-table :test 'equal))
  (dotimes (i 50000) (setf (gethash (format nil "k~D" i) *second*) (list i *first*)))
  (defun second-count () (hash-table-count *second*))
  ;; Old objects made to point at new ones: the saved core has to carry them.
  (setf (car *first*) "changed")
  (save-lisp-and-die "$second" :link t)
EOF
check_status_maybe_lose "link save over a link core" "$?" 0 "saved"

run_sbcl_with_core "$second" --noinform --disable-debugger \
    --no-userinit --no-sysinit --noprint <<EOF
  (assert (string= (car *first*) "changed"))
  (assert (= (first-sum) (+ (- (length "first 0")) (length "changed")
                            (loop for i below 100000 sum (length (format nil "first ~D" i))))))
  (assert (= (second-count) 50000))
  (assert (eq (second (gethash "k49999" *second*)) *first*))
  (setf (extern-alien "verify_gens" char) 0)
  (gc :full t)
  (assert (equal (first (gethash "k123" *second*)) 123))
  (exit :code $EXIT_LISP_WIN)
EOF
check_status_maybe_lose "boot a chain of link cores" "$?" "$EXIT_LISP_WIN" "loaded"

# Most of the second core is in its parents: it is much smaller than a
# monolithic save of the same heap.
run_sbcl_with_core "$second" --noinform --disable-debugger \
    --no-userinit --no-sysinit --noprint <<EOF
  (save-lisp-and-die "$monolithic")
EOF
check_status_maybe_lose "monolithic save for comparison" "$?" 0 "saved"
second_size=$(wc -c < "$second")
monolithic_size=$(wc -c < "$monolithic")
echo "link core $second_size bytes, monolithic core $monolithic_size bytes"
if [ $((second_size * 2)) -ge "$monolithic_size" ]; then
    echo "link core is not less than half the size of the monolithic core"
    exit $EXIT_LOSE
fi

# Replace the first core with another one. The second no longer loads.
run_sbcl <<EOF
  (save-lisp-and-die "$other" :link t)
EOF
check_status_maybe_lose "another link save" "$?" 0 "saved"
cp "$first" "$first.kept"
mv -f "$other" "$first"
run_sbcl_with_core "$second" --noinform --disable-debugger \
    --no-userinit --no-sysinit --noprint --eval '(exit :code 0)' \
    > link-core-replaced.out 2>&1
check_status_maybe_lose "link core with a replaced parent" "$?" 1 "refused"
grep -q "is no longer the core it was saved against" link-core-replaced.out || exit $EXIT_LOSE

# Remove it. The second does not load either.
rm -f "$first"
run_sbcl_with_core "$second" --noinform --disable-debugger \
    --no-userinit --no-sysinit --noprint --eval '(exit :code 0)' \
    > link-core-missing.out 2>&1
check_status_maybe_lose "link core with a missing parent" "$?" 1 "refused"
grep -q "cannot be opened" link-core-missing.out || exit $EXIT_LOSE

# Put the original back, and it loads again.
mv -f "$first.kept" "$first"
run_sbcl_with_core "$second" --noinform --disable-debugger \
    --no-userinit --no-sysinit --noprint <<EOF
  (assert (= (second-count) 50000))
  (exit :code $EXIT_LISP_WIN)
EOF
check_status_maybe_lose "link core with its parent restored" "$?" "$EXIT_LISP_WIN" "loaded"

rm -f "$first" "$second" "$monolithic" link-core-replaced.out link-core-missing.out
exit $EXIT_TEST_WIN
