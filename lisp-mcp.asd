(defsystem "lisp-mcp"
  :version "0.2.4"
  :description "Common Lisp MCP server: static analysis, structural editing, live Swank/Slynk images"
  :author "egao1980"
  :license "MIT"
  :depends-on ("alexandria" "bordeaux-threads"
               (:version "lisp-source-protocol" "0.2.2")
               (:version "repl-protocol" "0.2.5")
               (:version "repl-backend-swank" "0.2.2")
               (:version "repl-backend-slynk" "0.2.1")
               "process-backend-uiop"
               (:version "mcp-protocol" "0.3.0")
               (:version "mcp-backend-stdio" "0.2.0"))
  ;; repl-spawn images load swank / slynk with ASDF from where this image finds
  ;; them; the server never loads them, so they are install roots (cl-repo sync,
  ;; cl-repo run, cl-repo test), not dependencies.
  :properties (:cl-repo (:ci (:with ("swank" "slynk"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "state")
               (:file "cache")
               (:file "schema")
               (:file "render")
               (:file "notify")
               (:file "tools-source")
               (:file "tools-edit")
               (:file "tools-repl")
               (:file "server")
               (:file "main"))
  :in-order-to ((test-op (test-op "lisp-mcp/tests"))))

(defsystem "lisp-mcp/http"
  :description "Streamable HTTP transport for lisp-mcp"
  :depends-on ("lisp-mcp" (:version "mcp-backend-streamable-http" "0.3.0")))

(defsystem "lisp-mcp/tests"
  :depends-on ("lisp-mcp" "lisp-mcp/http" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "source-tools-test")
               (:file "resource-test")
               (:file "transport-test")
               (:file "repl-tools-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
