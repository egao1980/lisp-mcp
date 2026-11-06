(in-package #:lisp-mcp/tests)

(deftest live-image-session
  (with-project (root)
    (write-project-file root "demo.lisp" (format nil "(defun half (x) (/ x 2))~%"))
    (ok (search "No live image" (call "repl-eval" "code" "1")))
    (ok (search "SBCL" (call "repl-spawn")))
    (unwind-protect
         (progn
           (ok (search "=> 3" (call "repl-eval" "code" "(+ 1 2)")))
           (ok (search "path:0 ok (0 notes)" (call "repl-compile" "path" "demo.lisp" "locator" "defun half")))
           (let ((debug (call "repl-eval" "code" "(half \"x\")")))
             (ok (search "DEBUGGER level 1" debug))
             (ok (search "ABORT" debug)))
           (ok (search "HALF" (call "debug-backtrace" "end" 10) :test #'char-equal))
           (ok (search "Aborted" (call "debug-invoke-restart" "restart" 0)))
           (ok (search "Not in the debugger" (call "debug-state")))
           (let ((r (call "repl-eval" "code" "(list 1 2)")))
             (ok (search "result id" r))
             (let ((id (parse-integer r :start (+ (search "result id " r) 10) :junk-allowed t)))
               (ok (search "CONS" (call "inspect" "result" id) :test #'char-equal))))
           (ok (search "LIST" (call "repl-arglist" "symbol" "mapcar") :test #'char-equal)))
      (call "repl-disconnect"))))
