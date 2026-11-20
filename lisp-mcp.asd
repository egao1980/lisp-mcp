(defsystem "lisp-mcp"
  :version "0.6.1"
  :description "Common Lisp MCP server: static analysis, structural editing, live Swank/Slynk images"
  :author "egao1980"
  :license "MIT"
  :depends-on ("alexandria" "bordeaux-threads"
               (:version "lisp-source-protocol" "0.2.4")
               (:version "repl-protocol" "0.2.6")
               (:version "repl-backend-swank" "0.2.2")
               (:version "repl-backend-slynk" "0.2.1")
               "process-protocol"
               "process-backend-uiop"
               (:version "mcp-protocol" "0.3.1")
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
               (:file "scan-forms")
               (:file "schema")
               (:file "render")
               (:file "notify")
               (:file "tools-source")
               (:file "tools-edit")
               (:file "tools-repl")
               (:file "output")
               (:file "jobs")
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
               (:file "output-test")
               (:file "repl-tools-test")
               (:file "jobs-test")
               (:file "groups-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))

(defsystem "lisp-mcp/agent"
  :version "0.6.1"
  :description "lisp-mcp's tools as an ai-agent-protocol tool source (no MCP hop), with form-scanner permission resources and an observe mode"
  :author "egao1980"
  :license "MIT"
  :depends-on ("lisp-mcp"
               (:version "ai-agent-protocol" "0.12.0")
               (:version "llm-protocol" "0.5.0")
               (:version "json-protocol" "0.2.0")
               (:version "json-backend-jzon" "0.2.0"))
  ;; The live tests spawn images that load swank / slynk (see lisp-mcp).
  :properties (:cl-repo (:ci (:with ("swank" "slynk"))))
  :serial t
  :pathname "src/agent"
  :components ((:file "package")
               (:file "conditions")
               (:file "source"))
  :in-order-to ((test-op (test-op "lisp-mcp/agent/tests"))))

(defsystem "lisp-mcp/agent/tests"
  :depends-on ("lisp-mcp/agent" "lisp-mcp" "ai-agent-protocol" "llm-protocol" "json-protocol"
               "mcp-protocol" "repl-protocol" "process-protocol" "alexandria" "event-protocol" "event-backend-libuv" "rove")
  :pathname "tests/agent"
  :serial t
  :components ((:file "package")
               (:file "support")
               (:file "scan-test")
               (:file "construct-test")
               (:file "permissions-test")
               (:file "observe-test")
               (:file "live-test")
               (:file "agent-run-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
