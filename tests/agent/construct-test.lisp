(in-package #:lisp-mcp/agent/tests)

;;; MAKE-LISP-TOOLS, the catalog, static tool calls and errors (no image).

(defun catalog-names (groups)
  "lisp-mcp's tool names in its registration order, for GROUPS (keywords)."
  (loop for entry in (reverse lisp-mcp::*tools*)
        for name = (first entry)
        when (member (lisp-mcp::tool-group name) groups) collect name))

(defun read-kind-p (name)
  "True when lisp-mcp registered NAME as a read tool (readOnlyHint)."
  (and (fourth (find name lisp-mcp::*tools* :key #'first :test #'string=)) t))

(deftest construct-defaults
  (with-tools (tools root) '()
    (ok (lisp-tools-p tools))
    (ok (equal (namestring root) (namestring (lisp-tools-root tools))))
    (ok (equal lisp-mcp::*tool-groups* (lisp-tools-groups tools)))
    (ok (equal (catalog-names lisp-mcp::*tool-groups*) (lisp-tools-names tools)))
    (ok (eq :full (lisp-tools-mode tools)))
    (ok (null (lisp-tools-observe-allow tools)))
    (ok (eq t (lisp-tools-needs-approval tools)))
    (ok (string-equal "cl-user" (lisp-tools-package tools)))
    (ok (not (lisp-tools-closed-p tools)))))

(deftest construct-groups-and-tools
  (with-tools (tools root :groups "code") '()
    (ok (equal '(:source :edit :output) (lisp-tools-groups tools)))
    (ok (equal (catalog-names '(:source :edit :output)) (lisp-tools-names tools)))
    (ok (not (member "repl-eval" (lisp-tools-names tools) :test #'string=))))
  (with-tools (tools root :groups '(:repl :debug)) '()
    (ok (equal (catalog-names '(:repl :debug)) (lisp-tools-names tools))))
  (with-tools (tools root :tools '("source-read" "repl-eval")) '()
    (ok (equal '("source-read" "repl-eval") (lisp-tools-names tools)) "catalog order, not argument order"))
  (with-tools (tools root :package "my-app" :needs-approval nil) '()
    (ok (string-equal "my-app" (lisp-tools-package tools)))
    (ok (null (lisp-tools-needs-approval tools)))))

(deftest construct-errors
  (let ((root (fresh-directory)))
    (unwind-protect
         (progn
           (ok (real-failure (failure (make-lisp-tools :root root :groups "bogus"))))
           (ok (real-failure (failure (make-lisp-tools :root root :tools '("no-such-tool")))))
           (ok (real-failure (failure (make-lisp-tools :root root :mode :bogus))))
           (ok (real-failure (failure (make-lisp-tools :root (merge-pathnames "missing/" root))))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(deftest construct-observe-catalog
  (with-tools (tools root :mode :observe) '()
    (ok (eq :observe (lisp-tools-mode tools)))
    (let ((names (lisp-tools-names tools)))
      (ok (equal (remove-if-not (lambda (n) (or (read-kind-p n) (member n +observe-tools+ :test #'string=)))
                                (catalog-names lisp-mcp::*tool-groups*))
                 names)
          "read tools plus the observe tools, in catalog order")
      (ok (member "repl-eval" names :test #'string=))
      (ok (member "source-read" names :test #'string=))
      (ok (member "debug-backtrace" names :test #'string=))
      (dolist (n '("edit-replace" "edit-new-file" "repl-spawn" "repl-compile" "repl-load-system"
                   "repl-run-tests" "debug-invoke-restart" "debug-step" "inspect-action" "job-cancel"))
        (ok (not (member n names :test #'string=)) n)))))

(deftest descriptors
  (with-tools (tools root) '()
    (let ((descs (a:list-agent-tools tools)))
      (ok (equal (lisp-tools-names tools) (mapcar #'llm:llm-tool-name descs)))
      (ok (every (lambda (d) (equal "object" (gethash "type" (llm:llm-tool-parameters d)))) descs))
      (let* ((eval (find "repl-eval" descs :key #'llm:llm-tool-name :test #'string=))
             (props (gethash "properties" (llm:llm-tool-parameters eval))))
        (ok (equal (second (find "repl-eval" lisp-mcp::*tools* :key #'first :test #'string=))
                   (llm:llm-tool-description eval))
            "lisp-mcp's description")
        (ok (gethash "code" props))
        (ok (gethash "timeout" props) "job tools get timeout, as over MCP")
        (ok (gethash "verbose" props) "verbose tools get verbose, as over MCP")))
    (ok (a:tool-executable-p tools "repl-eval"))
    (ok (a:tool-executable-p tools "source-read"))
    (ok (not (a:tool-executable-p tools "nope"))))
  (with-tools (tools root :groups "code") '()
    (ok (not (a:tool-executable-p tools "repl-eval")))
    (let* ((read (find "source-outline" (a:list-agent-tools tools) :key #'llm:llm-tool-name :test #'string=))
           (props (gethash "properties" (llm:llm-tool-parameters read))))
      (ok (null (gethash "live" props)) "no image group: no image arguments, as over MCP")))
  (with-tools (tools root :mode :observe) '()
    (ok (not (a:tool-executable-p tools "edit-replace")))
    (ok (a:tool-executable-p tools "repl-eval"))))

(deftest static-calls-match-the-mcp-server
  (with-tools (tools root) `(("demo.lisp" ,+demo+))
    (let ((native (tool tools "source-outline" "path" "demo.lisp"))
          (server (lisp-mcp:make-lisp-mcp-server :root root)))
      (ok (has "half" native))
      (ok (has "quarter" native))
      (ok (equal native
                 (format nil "~{~A~}"
                         (map 'list (lambda (c) (gethash "text" c))
                              (gethash "content"
                                       (mcp-protocol:call-tool server "source-outline"
                                                               (mcp-protocol:json-object "path" "demo.lisp"))))))
          "same text as the MCP server"))
    (ok (has "(defun half (x) (/ x 2))" (tool tools "source-read" "path" "demo.lisp" "locator" "defun half")))
    (ok (stringp (tool tools "source-check" "path" "demo.lisp")))
    (ok (has "half" (call-lisp-tool tools "source-outline" (args "path" "demo.lisp")))
        "arguments as a hash table")
    (ok (has "half" (call-lisp-tool tools "source-outline" "{\"path\": \"demo.lisp\"}")))))

(deftest static-edit-writes-the-file
  (with-tools (tools root) `(("demo.lisp" ,+demo+))
    (ok (stringp (tool tools "edit-replace" "path" "demo.lisp" "locator" "defun half"
                       "code" "(defun half (x) (/ x 2.0))")))
    (ok (has "(/ x 2.0)" (read-project-file root "demo.lisp")))
    (ok (has "(defun quarter" (read-project-file root "demo.lisp")))))

(deftest call-errors
  (with-tools (tools root) `(("demo.lisp" ,+demo+))
    (let ((c (failure (tool tools "nope"))))
      (ok (typep c 'a:agent-unknown-tool)))
    (let ((c (failure (tool tools "source-read" "path" "../outside.lisp" "locator" "defun x"))))
      (ok (typep c 'a:tool-input-error))
      (ok (has "outside" (message c))))
    (let ((c (failure (tool tools "source-read"))))
      (ok (typep c (quote a:tool-input-error)) "missing path"))
    (let ((c (failure (tool tools "source-read" "path" "demo.lisp" "locator" "defun nope"))))
      (ok (typep c 'a:tool-input-error) "locator miss"))
    (let ((c (failure (tool tools "repl-eval" "code" "(+ 1 2)"))))
      (ok (typep c (quote a:tool-input-error)) "no image: lisp-mcp says to spawn one")
      (ok (has "No live image" (message c))))
    (let ((c (failure (tool tools "repl-eval" "code" "1" "connection" "ghost"))))
      (ok (typep c (quote a:tool-input-error)) "unknown connection"))
    (let ((c (failure (call-lisp-tool tools "source-outline" "{not json"))))
      (ok (typep c 'a:tool-input-error) "bad JSON"))
    (let ((c (failure (call-lisp-tool tools "source-outline" "[1, 2]"))))
      (ok (typep c 'a:tool-input-error) "not an object")))
  (with-tools (tools root :groups "code") '()
    (ok (typep (failure (tool tools "repl-eval" "code" "1")) 'a:agent-unknown-tool)
        "a tool outside the groups is unknown")))

(deftest long-output-is-stored
  (with-tools (tools root)
      `(("big.lisp" ,(with-output-to-string (s)
                       (dotimes (i 300) (format s "(defun f~D () ~D)~%" i i)))))
    (let ((outline (tool tools "source-outline" "path" "big.lisp")))
      (ok (has "Stored as o1" outline))
      (ok (has "f299" (tool tools "output-read" "id" "o1" "grep" "f299"))))))

(deftest instructions
  (with-tools (tools root) '()
    (let ((text (lisp-tools-instructions tools)))
      (ok (has "SOURCE (read only" text))
      (ok (has "repl-spawn" text))
      (ok (not (has "Resources:" text)))
      (ok (not (has "OBSERVE MODE" text)))))
  (with-tools (tools root :groups "code") '()
    (let ((text (lisp-tools-instructions tools)))
      (ok (has "EDIT" text))
      (ok (not (has "LIVE" text)))))
  (with-tools (tools root :mode :observe) '()
    (let ((text (lisp-tools-instructions tools)))
      (ok (find-if (lambda (line) (alexandria:starts-with-subseq "OBSERVE MODE" line))
                   (uiop:split-string text :separator '(#\Newline)))))))

(deftest close-is-final
  (with-tools (tools root) `(("demo.lisp" ,+demo+))
    (ok (null (close-lisp-tools tools)))
    (ok (lisp-tools-closed-p tools))
    (ok (null (close-lisp-tools tools)) "idempotent")
    (ok (typep (failure (tool tools "source-outline" "path" "demo.lisp")) 'a:tool-unavailable))))
