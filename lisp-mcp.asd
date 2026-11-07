(defsystem "lisp-mcp"
  :version "0.2.0"
  :description "Common Lisp MCP server: static analysis, structural editing, live Swank/Slynk images"
  :author "egao1980"
  :license "MIT"
  :depends-on ("alexandria" "bordeaux-threads"
               (:version "lisp-source-protocol" "0.2.0")
               (:version "repl-protocol" "0.2.0")
               (:version "repl-backend-swank" "0.2.0")
               (:version "repl-backend-slynk" "0.2.0")
               "process-backend-uiop"
               (:version "mcp-protocol" "0.3.0")
               (:version "mcp-backend-stdio" "0.2.0"))
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
