(defsystem "lisp-mcp"
  :version "0.1.0"
  :description "Common Lisp MCP server: static analysis, structural editing, live Swank/Slynk images"
  :author "egao1980"
  :license "MIT"
  :depends-on ("alexandria" "bordeaux-threads" "lisp-source-protocol" "repl-protocol" "repl-backend-swank" "repl-backend-slynk"
               "process-backend-uiop" "mcp-protocol" "mcp-backend-stdio")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "state")
               (:file "schema")
               (:file "render")
               (:file "tools-source")
               (:file "tools-edit")
               (:file "tools-repl")
               (:file "server")
               (:file "main"))
  :in-order-to ((test-op (test-op "lisp-mcp/tests"))))

(defsystem "lisp-mcp/http"
  :description "Streamable HTTP transport for lisp-mcp"
  :depends-on ("lisp-mcp" "mcp-backend-streamable-http"))

(defsystem "lisp-mcp/tests"
  :depends-on ("lisp-mcp" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "source-tools-test")
               (:file "repl-tools-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
