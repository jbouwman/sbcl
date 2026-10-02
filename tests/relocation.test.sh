#!/bin/sh

. ./subr.sh

# The relocation test binary can only be built on linux or x86-64 + darwin.
data=`run_sbcl --eval '(progn #+(or linux (and x86-64 darwin))(progn(princ "fakemap") #+64-bit(princ "_64")))' \
  --quit`
if [ -z "$data" ]
then
    # shell tests don't have a way of exiting as "not applicable"
    exit $EXIT_TEST_WIN
fi

test_sbcl=../src/runtime/heap-reloc-test

rm -f $test_sbcl

set -e
(cd ../src/runtime ; make heap-reloc-test)

# Exercise all the lines of 'fakemap' by starting up N times in a row.
# KLUDGE: assume N = 6
# FIXME: don't assume that N = 6

export SBCL_FAKE_MMAP_INSTRUCTION_FILE=`pwd`/heap-reloc/$data
i=1
while [ $i -le 6 ]
do
  export SBCL_FAKE_MMAP_INSTRUCTION_LINE=$i
  $test_sbcl --lose-on-corruption --disable-ldb --noinform --core ../output/sbcl.core \
              --no-sysinit --no-userinit --noprint --disable-debugger \
              --eval '(gc :full t)' \
              --eval '(defun fib (n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))' \
              --eval "(compile 'fib)" -quit
  i=`expr $i + 1`
done

create_test_subdirectory
tmpcore=$TEST_DIRECTORY/$TEST_FILESTEM.core

run_sbcl <<EOF
  (defglobal original-static-space-bounds
    (cons sb-vm:static-space-start (sb-sys:sap-int sb-vm:*static-space-free-pointer*)))
  ;; there's no point in testing for #-x86-64. While arm64 allows #+relocatable-static-space
  ;; it only does so if #+immobile-space which is not the default config
  #+x86-64
  (when (member :alien-callbacks sb-impl:+internal-features+)
    (push :do-test *features*)
    (sb-alien:define-alien-callable foo int () 42))
  (save-lisp-and-die "$tmpcore")
EOF

$test_sbcl --lose-on-corruption --disable-ldb --noinform --core $tmpcore \
              --no-sysinit --no-userinit --noprint --disable-debugger <<EOF
#-do-test (quit)
;; check that static space relocation happened
(assert (not (eql sb-vm:static-space-start (car original-static-space-bounds))))
;; the identical alien is stored in two places (so there is 1 and only 1 SAP)
(assert (eq (aref sb-alien::*alien-callbacks* 0)
            (gethash 'foo sb-alien::*alien-callables*)))
;; the SAP points within static space
(let* ((alien (aref sb-alien::*alien-callbacks* 0))
       (sap (alien-sap alien)))
 (assert (sb-sys:sap>= sap (sb-sys:int-sap sb-vm:static-space-start)))
 (assert (sb-sys:sap< sap sb-vm:*static-space-free-pointer*)))
;; the callable doesn't crash
(let ((result (alien-funcall (sb-alien:alien-callable-function 'foo))))
  (assert (= result 42)))
(format t "~&I'm back!~%")
EOF

# A link core loaded at other addresses: the pages it takes from its parents
# are relocated with the rest. A link save from a relocated process compares
# its pages as relocated, so the ones relocation changed are written and the
# result loads at its own addresses and at others.
link=`run_sbcl --eval '(when (sb-impl::link-save-supported-p) (princ "link"))' --quit`
if [ "$link" = link ]; then
  first=$TEST_DIRECTORY/$TEST_FILESTEM-first.core
  second=$TEST_DIRECTORY/$TEST_FILESTEM-second.core
  third=$TEST_DIRECTORY/$TEST_FILESTEM-third.core
  run_sbcl <<EOF
    (defvar *first* (loop for i below 100000 collect (format nil "first ~D" i)))
    (save-lisp-and-die "$first" :link t)
EOF
  run_sbcl_with_core "$first" --noinform --no-sysinit --no-userinit --noprint \
                     --disable-debugger <<EOF
    (defvar *second* (make-hash-table :test 'equal))
    (dotimes (i 50000) (setf (gethash (format nil "k~D" i) *second*) (list i *first*)))
    (setf (car *first*) "changed")
    (save-lisp-and-die "$second" :link t)
EOF
  i=1
  while [ $i -le 6 ]
  do
    export SBCL_FAKE_MMAP_INSTRUCTION_LINE=$i
    $test_sbcl --lose-on-corruption --disable-ldb --noinform --core "$second" \
               --no-sysinit --no-userinit --noprint --disable-debugger <<EOF
      (assert (string= (car *first*) "changed"))
      (assert (eq (second (gethash "k49999" *second*)) *first*))
      (setf (extern-alien "verify_gens" char) 0)
      (gc :full t)
      (assert (equal (first (gethash "k123" *second*)) 123))
EOF
    i=`expr $i + 1`
  done

  export SBCL_FAKE_MMAP_INSTRUCTION_LINE=2
  $test_sbcl --lose-on-corruption --disable-ldb --noinform --core "$second" \
             --no-sysinit --no-userinit --noprint --disable-debugger <<EOF
    (defvar *third*
      (loop for i below 20000 collect (cons i (gethash (format nil "k~D" i) *second*))))
    (save-lisp-and-die "$third" :link t)
EOF
  check_third='(progn
    (assert (string= (car *first*) "changed"))
    (assert (eq (third (nth 19999 *third*)) *first*))
    (assert (= (hash-table-count *second*) 50000))
    (setf (extern-alien "verify_gens" char) 0)
    (gc :full t)
    (assert (equal (second (nth 123 *third*)) 123)))'
  run_sbcl_with_core "$third" --noinform --no-sysinit --no-userinit --noprint \
                     --disable-debugger --eval "$check_third" --quit
  for i in 1 6
  do
    export SBCL_FAKE_MMAP_INSTRUCTION_LINE=$i
    $test_sbcl --lose-on-corruption --disable-ldb --noinform --core "$third" \
               --no-sysinit --no-userinit --noprint --disable-debugger \
               --eval "$check_third" --quit
  done
  rm -f "$first" "$second" "$third"
fi

rm -f $tmpcore $test_sbcl

exit $EXIT_TEST_WIN
