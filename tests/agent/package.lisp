(defpackage #:lisp-mcp/agent/tests
  (:use #:cl #:rove #:lisp-mcp/agent)
  (:import-from #:lisp-mcp #:form-resources)
  (:local-nicknames (#:a #:ai-agent-protocol)
                    (#:llm #:llm-protocol)
                    (#:json #:json-protocol)
                    (#:repl #:repl-protocol)))

(in-package #:lisp-mcp/agent/tests)
