(in-package #:lisp-mcp/agent/tests)

;;; Live images: repl-spawn starts ros +Q run with Swank or Slynk, as in
;;; lisp-mcp's own tests (swank / slynk are :ci :with roots).

(defparameter +seeded+
  "(defun seeded (x) (restart-case (error \"seeded bug ~a\" x) (return-default () :report \"Return 42\" 42)))")

(deftest-parametrize live-eval-debugger-restarts
    ((backend) :ids ("swank" "slynk") ("swank") ("slynk"))
  (with-tools (tools root) `(("demo.lisp" ,+demo+))
    (ok (has "SBCL" (tool tools "repl-spawn" "backend" backend)))
    (ok (has "=> 3" (tool tools "repl-eval" "code" "(+ 1 2)")))
    (ok (has "ok" (tool tools "repl-compile" "path" "demo.lisp" "locator" "defun half")))
    (ok (has "=> 2" (tool tools "repl-eval" "code" "(half 4)")) "the compiled form is live")
    (ok (has "SEEDED" (tool tools "repl-eval" "code" +seeded+)))
    (let ((debug (tool tools "repl-eval" "code" "(seeded 1)")))
      (ok (has "DEBUGGER level 1" debug))
      (ok (has "RETURN-DEFAULT" debug)))
    (ok (equal '("restart:return-default") (resources tools "debug-invoke-restart" "restart" 0)))
    (ok (equal '("restart:abort") (resources tools "debug-invoke-restart" "restart" 1))
        "the *ABORT marker is dropped")
    (ok (equal '("*") (resources tools "debug-invoke-restart" "restart" 99)) "out of range")
    (ok (has "=> 42" (tool tools "debug-invoke-restart" "restart" 0)))
    (ok (has "Not in the debugger" (tool tools "debug-state")))
    (ok (equal '("*") (resources tools "debug-invoke-restart" "restart" 0)) "no debugger")))

(deftest live-background-job
  (with-tools (tools root) '()
    (tool tools "repl-spawn")
    (let ((r (tool tools "repl-eval" "code" "(progn (sleep 3) :done)" "timeout" 1)))
      (ok (has "Still running as job j1" r))
      (ok (has ":DONE" (tool tools "job-wait" "id" "j1" "timeout" 30))))))

(deftest live-close-kills-spawned-images
  (with-tools (tools root) '()
    (tool tools "repl-spawn")
    (let ((connections (alexandria:hash-table-values
                        (lisp-mcp::state-connections (lisp-mcp/agent::lisp-tools-state tools)))))
      (ok (= 1 (length connections)))
      (close-lisp-tools tools)
      (ok (notany #'repl:repl-connected-p connections))
      (ok (typep (failure (tool tools "repl-list")) 'a:tool-unavailable)))))

(deftest live-observe-attached-image
  (with-tools (full root) '()
    (tool full "repl-spawn")
    (let* ((r (tool full "repl-eval" "code" "(swank:create-server :port 0 :dont-close t)"))
           (port (parse-integer r :start (+ 3 (search "=> " r)) :junk-allowed t)))
      (ok (integerp port))
      (with-tools (watch root2 :mode :observe) '()
        (ok (stringp (tool watch "repl-connect" "port" port)))
        (ok (has "=> 3" (tool watch "repl-eval" "code" "(+ 1 2)")))
        (ok (has "=> " (tool watch "repl-eval" "code" "(length *features*)")))
        (ok (typep (failure (tool watch "repl-eval" "code" "(defparameter *observed* 1)"))
                   'observe-mode-refused))
        (ok (has "=> NIL" (tool full "repl-eval" "code" "(boundp '*observed*)"))
            "the refused form never reached the image")
        (close-lisp-tools watch))
      (ok (has "=> 3" (tool full "repl-eval" "code" "(+ 1 2)"))
          "closing an observer does not stop an attached image"))))
