(defpackage #:lisp-mcp/agent
  (:use #:cl)
  (:local-nicknames (#:agent #:ai-agent-protocol)
                    (#:llm #:llm-protocol)
                    (#:json #:json-protocol)
                    (#:repl #:repl-protocol)
                    (#:lm #:lisp-mcp)
                    (#:src #:lisp-source-protocol))
  (:export
   ;; tool source
   #:lisp-tools
   #:lisp-tools-p
   #:make-lisp-tools
   #:lisp-tools-root
   #:lisp-tools-groups
   #:lisp-tools-names
   #:lisp-tools-mode
   #:lisp-tools-observe-allow
   #:lisp-tools-needs-approval
   #:lisp-tools-package
   #:lisp-tools-closed-p
   #:call-lisp-tool
   #:close-lisp-tools
   #:lisp-tools-instructions
   ;; tool classes
   #:+approval-tools+
   #:+code-tools+
   #:+observe-tools+
   #:observe-safe-resources-p
   ;; conditions
   #:observe-mode-refused))

(in-package #:lisp-mcp/agent)
