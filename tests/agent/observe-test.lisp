(in-package #:lisp-mcp/agent/tests)

;;; :OBSERVE mode without an image: the gate runs before the call, so code
;;; that passes it reaches lisp-mcp (and fails there with "No live image",
;;; a TOOL-INPUT-ERROR).

(deftest observe-safe-resources
  (ok (observe-safe-resources-p '("lisp:pure")))
  (ok (observe-safe-resources-p '("lisp:reads cl:*features*" "lisp:reads my-app:*state*")))
  (ok (not (observe-safe-resources-p '("lisp:io"))))
  (ok (not (observe-safe-resources-p '("lisp:reads cl:*x*" "lisp:mutates cl-user:*x*"))))
  (ok (not (observe-safe-resources-p '("lisp:*"))))
  (ok (not (observe-safe-resources-p '("*"))))
  (ok (not (observe-safe-resources-p '("lisp:calls my-app:status"))))
  (ok (observe-safe-resources-p '("lisp:calls my-app:status") '("lisp:calls my-app:*")))
  (ok (not (observe-safe-resources-p '("lisp:calls other:status") '("lisp:calls my-app:*")))))

(defun refusal (form)
  "The OBSERVE-MODE-REFUSED FORM signals, else NIL."
  (let ((c (failure form)))
    (and (typep c 'observe-mode-refused) c)))

(deftest observe-refuses-mutating-code
  (with-tools (tools root :mode :observe) '()
    (let ((c (refusal (tool tools "repl-eval" "code" "(setf *x* 1)"))))
      (ok c)
      (ok (typep c 'a:tool-permission-denied))
      (ok (equal "repl-eval" (and c (a:tool-permission-denied-action c))))
      (ok (equal '("lisp:mutates cl-user:*x*") (and c (a:tool-permission-denied-resources c))))
      (ok (has "observe" (and c (a:tool-permission-denied-feedback c)))))
    (let ((c (refusal (tool tools "repl-eval" "code" "(progn (print 1) (+ 1 2))"))))
      (ok (equal '("lisp:io") (and c (a:tool-permission-denied-resources c)))
          "only the offending resources"))
    (ok (refusal (tool tools "repl-eval" "code" "(defun f () 1)")))
    (ok (refusal (tool tools "repl-eval" "code" "(my-app:reset)")))
    (ok (refusal (tool tools "repl-eval" "code" "(funcall f)")))
    (ok (refusal (tool tools "repl-eval" "code" "#.(+ 1 2)")))
    (ok (refusal (tool tools "repl-eval" "code" "(+ 1")))
    (ok (refusal (tool tools "debug-eval-in-frame" "frame" 0 "code" "(setf *x* 1)")))
    (ok (refusal (tool tools "inspect" "code" "(delete-file \"x\")")))))

(deftest observe-passes-safe-code
  (with-tools (tools root :mode :observe) '()
    (dolist (code '("(+ 1 2)" "(length *features*)" "(with-output-to-string (s) (princ 1 s))"))
      (let ((c (failure (tool tools "repl-eval" "code" code))))
        (ok (typep c 'a:tool-input-error) code)))
    (ok (typep (failure (tool tools "inspect" "result" 1)) 'a:tool-input-error))
    (ok (not (approval-p tools "repl-eval" "code" "(+ 1 2)"))))
  (with-tools (tools root :mode :observe :observe-allow '("lisp:calls my-app:*-status")) '()
    (ok (typep (failure (tool tools "repl-eval" "code" "(my-app:server-status)")) 'a:tool-input-error))
    (ok (refusal (tool tools "repl-eval" "code" "(my-app:reset)")))))

(deftest observe-refuses-other-tools
  (with-tools (tools root :mode :observe) `(("demo.lisp" ,+demo+))
    (dolist (name '("edit-replace" "edit-new-file" "repl-spawn" "repl-compile" "repl-load-system"
                    "repl-run-tests" "debug-invoke-restart" "inspect-action" "job-cancel"))
      (let ((c (refusal (tool tools name "path" "demo.lisp" "locator" "defun half"
                              "code" "(defun half (x) x)"))))
        (ok c name)
        (ok (equal name (and c (a:tool-permission-denied-action c))))))
    (ok (equal +demo+ (read-project-file root "demo.lisp")) "the file is untouched")
    (ok (has "half" (tool tools "source-outline" "path" "demo.lisp")) "read tools run")
    (ok (typep (failure (tool tools "nope")) 'a:agent-unknown-tool) "unknown stays unknown")))
