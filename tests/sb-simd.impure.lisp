;;; Under the evaluator
;;; (SB-SIMD-TEST-SUITE::|SB-SIMD-SSE4.1:U32.4-IF|)
;;; failed for me with
;;;    RESULT-0 = #<SIMD-PACK   33554433        127       8191    2097153>,
;;;    OUTPUT-0 = #<SIMD-PACK          3       8193       2303 1073741823>.
;;; maybe there are other problems but I didn't investigate further.
#+(or interpreter gc-stress) (invoke-restart 'run-tests::skip-file)

(handler-case (require :sb-simd)
  (condition (c)
    (cond ((search "Don't know how" (princ-to-string c))
           (format t "~&Skipping test of sb-simd~%")
           (invoke-restart 'run-tests::skip-file))
          (t
           (error "Unexpected error: ~A" c)))))

(with-compilation-unit ()
  (dolist (file '("packages.lisp"
                  "numbers.lisp"
                  "utilities.lisp"
                  "test-suite.lisp"
                  "test-arefs.lisp"
                  #+x86-64 "test-arefs-x86-64.lisp"
                  #+arm64 "test-arefs-arm64.lisp"
                  "test-simple-simd-functions.lisp"
                  #+x86-64 "test-simple-simd-functions-x86-64.lisp"
                  #+arm64 "test-simple-simd-functions-arm64.lisp"
                  "test-horizontal-functions.lisp"
                  #+x86-64 "test-horizontal-functions-x86-64.lisp"
                  #+arm64 "test-horizontal-functions-arm64.lisp"
                  #+arm64 "test-arm64-regressions.lisp"
                  "test-hairy-simd-functions.lisp"
                  "test-packages.lisp"))
    (load (merge-pathnames file #P"../contrib/sb-simd/test-suite/"))))
(sb-simd-test-suite::run-test-suite)

;;; The AVX lane extractions copy lane 0 with a VEX-encoded move. A legacy
;;; MOVSS or MOVSD between XMM registers would cost an AVX-to-SSE transition
;;; while the upper YMM halves are dirty, and every AVX horizontal reduction
;;; and every *-VALUES ends in one of these.
#+x86-64
(with-test (:name (:sb-simd :avx :lane-extraction-is-vex-encoded))
  (dolist (name '(sb-simd-avx:f32.4-horizontal+ sb-simd-avx:f32.8-horizontal+
                  sb-simd-avx:f64.2-horizontal+ sb-simd-avx:f64.4-horizontal+
                  sb-simd-avx:f64.4-values))
    (with-input-from-string (s (with-output-to-string (out)
                                 (disassemble name :stream out)))
      (loop for line = (read-line s nil)
            while line
            when (and (or (search " MOVSS " line) (search " MOVSD " line))
                      (search ", XMM" line)
                      (not (search "[" line)))
              do (error "~S moves between registers with a legacy instruction:~%~A"
                        name line)))))
