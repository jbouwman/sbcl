#!/bin/sh
# This software is in the public domain. See COPYING and CREDITS.
. ./subr.sh
use_test_subdirectory

cat > sigterm.lisp <<'EOF'
#+(and unix sb-thread (not sb-safepoint))
(let* ((receiver (second sb-ext:*posix-argv*))
       (thread (cond ((string= receiver "main") sb-thread:*current-thread*)
                     ((string= receiver "finalizer") sb-impl::*finalizer-thread*)
                     (t (sb-thread:make-thread (lambda () (sleep 60)))))))
  ;; A hook proves that normal process shutdown ran on the main thread.
  (push (lambda ()
          (unless (sb-thread::main-thread-p)
            (sb-ext:exit :code 9 :abort t))
          (write-line "SIGTERM-EXIT-HOOK"))
        sb-ext:*exit-hooks*)
  (sb-unix:pthread-kill (sb-thread::thread-os-thread thread) sb-unix:sigterm)
  (sleep 10)
  (sb-ext:exit :code 8 :abort t))
#-(and unix sb-thread (not sb-safepoint))
(sb-ext:exit :code 52)
EOF

for receiver in main worker finalizer; do
    "$SBCL_RUNTIME" --core "$SBCL_CORE" $SBCL_ARGS --script sigterm.lisp "$receiver" > output &
    child=$!
    # A stuck exit lock must fail this test rather than hang the suite.
    (sleep 15; kill -KILL "$child" 2>/dev/null) &
    watchdog=$!
    wait "$child"
    status=$?
    kill "$watchdog" 2>/dev/null || true
    wait "$watchdog" 2>/dev/null || true
    if [ "$status" = 52 ]; then exit 104; fi
    check_status_maybe_lose "SIGTERM to $receiver" "$status" 0 "process exited"
    grep -q '^SIGTERM-EXIT-HOOK$' output || exit 1
done
exit 104
