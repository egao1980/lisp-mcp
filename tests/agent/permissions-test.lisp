(in-package #:lisp-mcp/agent/tests)

;;; TOOL-PERMISSION-RESOURCES, TOOL-NEEDS-APPROVAL-P and how rules read them
;;; (no image).

(defparameter +my-app-file+
  (format nil "(in-package :my-app)~%~%(defun start () (print :up))~%"))

(deftest resources-code-tools
  (with-tools (tools root) '()
    (ok (equal '("lisp:pure") (resources tools "repl-eval" "code" "(+ 1 2)")))
    (ok (equal '("lisp:mutates cl-user:*x*") (resources tools "repl-eval" "code" "(setf *x* 1)")))
    (ok (equal '("lisp:calls my-app:foo") (resources tools "repl-eval" "code" "(foo)" "package" "my-app"))
        "the call's package")
    (ok (equal '("lisp:io") (resources tools "debug-eval-in-frame" "frame" 0 "code" "(print 1)")))
    (ok (equal '("lisp:pure") (resources tools "inspect" "code" "(list 1 2)")))
    (ok (equal '("inspect:3") (resources tools "inspect" "result" 3)))
    (ok (equal '("*") (resources tools "repl-eval" "code" "")) "blank code")
    (ok (equal '("*") (resources tools "repl-eval")) "no code")
    (ok (equal '("*") (a:tool-permission-resources tools "repl-eval" "{not json")) "never signals"))
  (with-tools (tools root :package "my-app") '()
    (ok (equal '("lisp:calls my-app:foo") (resources tools "repl-eval" "code" "(foo)"))
        "the source's package when the call names none")))

(deftest resources-path-tools
  (with-tools (tools root) `(("demo.lisp" ,+demo+))
    (ok (equal '("path:demo.lisp") (resources tools "source-read" "path" "demo.lisp" "locator" "defun half")))
    (ok (equal '("path:demo.lisp") (resources tools "source-outline" "path" "sub/../demo.lisp")))
    (ok (equal '("path:../x.lisp") (resources tools "source-read" "path" "../x.lisp"))
        "a path outside the root is kept as given")
    (ok (equal '("*") (resources tools "source-find-definitions" "symbol" "half")) "no path")
    (ok (equal '("path:demo.lisp") (resources tools "edit-replace" "path" "demo.lisp" "locator" "defun half"
                                              "code" "(defun half (x) x)")))
    (ok (resources= (resources tools "edit-replace" "path" "demo.lisp" "locator" "defun half"
                               "code" "(defun half (x) (print x))" "compile" t)
                    '("path:demo.lisp" "lisp:defines cl-user:half" "lisp:io"))
        "compile=true scans the new code")))

(deftest resources-compile
  (with-tools (tools root) `(("demo.lisp" ,+demo+) ("app.lisp" ,+my-app-file+))
    (ok (resources= (resources tools "repl-compile" "path" "demo.lisp" "locator" "defun half")
                    '("path:demo.lisp" "lisp:defines cl-user:half")))
    (ok (resources= (resources tools "repl-compile" "path" "demo.lisp")
                    '("path:demo.lisp" "lisp:defines cl-user:half" "lisp:defines cl-user:quarter"
                      "lisp:calls cl-user:half"))
        "the whole file")
    (ok (resources= (resources tools "repl-compile" "path" "app.lisp" "locator" "defun start")
                    '("path:app.lisp" "lisp:defines my-app:start" "lisp:io"))
        "the package in effect at the form")
    (ok (member "lisp:*" (resources tools "repl-compile" "path" "../x.lisp") :test #'string=)
        "a file that does not resolve")
    (ok (member "lisp:*" (resources tools "repl-compile" "path" "demo.lisp" "locator" "defun nope")
                :test #'string=)
        "a locator that misses")))

(deftest resources-edit-compile-in-the-file-package
  ;; #17: edit-* with compile=true scans `code` in the package in effect at the
  ;; located form, as the edit compiles it (and as repl-compile scans).
  (with-tools (tools root) `(("app.lisp" ,+my-app-file+) ("demo.lisp" ,+demo+))
    (ok (resources= (resources tools "edit-replace" "path" "app.lisp" "locator" "defun start"
                               "code" "(defun start () (print :go))" "compile" t)
                    '("path:app.lisp" "lisp:defines my-app:start" "lisp:io"))
        "edit-replace: the file's in-package, not cl-user")
    (ok (resources= (resources tools "edit-insert" "path" "app.lisp" "locator" "defun start"
                               "code" "(defun stop () 1)" "compile" t)
                    '("path:app.lisp" "lisp:defines my-app:stop"))
        "edit-insert after the form")
    (ok (resources= (resources tools "edit-insert" "path" "app.lisp" "locator" "path:0"
                               "where" "after" "code" "(defun stop () 1)" "compile" t)
                    '("path:app.lisp" "lisp:defines my-app:stop"))
        "after the in-package form: its package")
    (ok (resources= (resources tools "edit-insert" "path" "app.lisp" "locator" "path:0"
                               "where" "before" "code" "(defun stop () 1)" "compile" t)
                    '("path:app.lisp" "lisp:defines cl-user:stop"))
        "before the in-package form: the package before it")
    (ok (resources= (resources tools "edit-replace" "path" "demo.lisp" "locator" "defun half"
                               "code" "(defun half (x) x)" "compile" t)
                    '("path:demo.lisp" "lisp:defines cl-user:half"))
        "no in-package: cl-user")
    (ok (resources= (resources tools "edit-replace" "path" "app.lisp" "locator" "defun nope"
                               "code" "(defun nope () 1)" "compile" t)
                    '("path:app.lisp" "lisp:defines cl-user:nope"))
        "a locator that misses: the call's or source's package"))
  (with-tools (tools root :package "my-app") `(("demo.lisp" ,+demo+))
    (ok (resources= (resources tools "edit-replace" "path" "demo.lisp" "locator" "defun half"
                               "code" "(defun half (x) x)" "compile" t)
                    '("path:demo.lisp" "lisp:defines cl-user:half"))
        "the file decides, as the compile does, not the source's :package")))

(deftest resources-image-tools
  (with-tools (tools root) '()
    (ok (equal '("system:my-app") (resources tools "repl-load-system" "system" "My-App")))
    (ok (equal '("system:my-app/tests") (resources tools "repl-run-tests" "system" "my-app/tests")))
    (ok (equal '("image:spawn") (resources tools "repl-spawn")))
    (ok (equal '("image:spawn") (resources tools "repl-spawn" "backend" "slynk")))
    (ok (equal '("image:127.0.0.1:4005") (resources tools "repl-connect" "port" 4005)))
    (ok (equal '("image:example.org:4006") (resources tools "repl-connect" "host" "example.org" "port" 4006)))
    (ok (equal '("*") (resources tools "debug-invoke-restart" "restart" 0))
        "no image: the restart is unknown")))

(deftest approval-defaults
  (with-tools (tools root) `(("demo.lisp" ,+demo+))
    (ok (not (approval-p tools "repl-eval" "code" "(+ 1 2)")) "pure code runs")
    (ok (not (approval-p tools "repl-eval" "code" "(length *features*)")) "reads run")
    (ok (approval-p tools "repl-eval" "code" "(setf *x* 1)"))
    (ok (approval-p tools "repl-eval" "code" "(print 1)"))
    (ok (approval-p tools "repl-eval" "code" "(my-app:start)"))
    (ok (approval-p tools "repl-eval" "code" "(+ 1"))
    (ok (approval-p tools "repl-eval") "no code: lisp-mcp reports it, but the scan cannot clear it")
    (ok (not (approval-p tools "inspect" "result" 1)))
    (ok (approval-p tools "inspect" "code" "(setf *x* 1)"))
    (ok (not (approval-p tools "debug-eval-in-frame" "frame" 0 "code" "(+ 1 2)")))
    (dolist (name +approval-tools+)
      (ok (approval-p tools name) name))
    (dolist (name '("source-read" "source-outline" "source-check" "edit-replace" "edit-new-file"
                    "repl-disconnect" "repl-list" "debug-state" "debug-backtrace" "job-wait"
                    "job-cancel" "output-read" "repl-interrupt"))
      (ok (not (approval-p tools name)) name))
    (ok (approval-p tools "edit-replace" "path" "demo.lisp" "locator" "defun half"
                    "code" "(defun half (x) x)" "compile" t)
        "compile=true: a definition asks"))
  (with-tools (tools root :needs-approval nil) '()
    (ok (not (approval-p tools "repl-eval" "code" "(setf *x* 1)")))
    (ok (not (approval-p tools "repl-spawn")))))

(defun permission (tools rules name &rest kv)
  "AGENT-PERMISSION effect of an agent with RULES calling NAME on TOOLS."
  (let ((agent (a:make-ai-agent :name "lisper" :backend (llm:make-mock-llm-backend)
                                :tools (list tools) :permissions rules)))
    (values (a:agent-permission agent tools name (apply #'json-args kv)))))

(deftest rules-over-scanner-output
  (with-tools (tools root) '()
    (ok (eq :allow (permission tools '() "repl-eval" "code" "(+ 1 2)")))
    (ok (eq :ask (permission tools '() "repl-eval" "code" "(delete-file \"x\")")))
    (let ((rules '(("repl-eval" "lisp:calls my-app:*" :allow))))
      (ok (eq :allow (permission tools rules "repl-eval" "code" "(my-app:start)")))
      (ok (eq :ask (permission tools rules "repl-eval" "code" "(my-app:start) (print 1)"))
          "an undecided io resource falls back to the tool's ask")
      (ok (eq :ask (permission tools rules "repl-eval" "code" "(funcall (my-app:pick))"))
          "lisp:* is only matched by a catch-all"))
    (ok (eq :deny (permission tools '(("repl-eval" "lisp:defines *" :deny)) "repl-eval"
                              "code" "(progn (defun f () 1) (+ 1 2))")))
    (ok (eq :ask (permission tools '(("repl-eval" "lisp:io" :ask) ("repl-eval" "lisp:pure" :allow))
                             "repl-eval" "code" "(print 1)")))
    (ok (eq :deny (permission tools '(("*" "*" :allow) ("repl-eval" "lisp:read-eval" :deny))
                              "repl-eval" "code" "#.(+ 1 2)")))
    (ok (eq :allow (permission tools '(("repl-eval" "lisp:*" :allow)) "repl-eval" "code" "(funcall f)"))
        "a lisp:* rule glob is a catch-all")
    (ok (eq :allow (permission tools '(("repl-load-system" "system:my-app" :allow))
                               "repl-load-system" "system" "my-app")))
    (ok (eq :ask (permission tools '(("repl-load-system" "system:my-app" :allow))
                             "repl-load-system" "system" "other")))
    (ok (eq :allow (permission tools '(("*" "path:src/*" :allow)) "source-read" "path" "src/a.lisp")))))
