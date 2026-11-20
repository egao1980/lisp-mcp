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
   ;; H4b: image events (the debugger → agent inbox bridge)
   #:lisp-tools-image-events-p
   #:lisp-tools-on-image-event
   #:image-event
   #:image-event-p
   #:image-event-kind
   #:image-event-connection
   #:image-event-thread
   #:image-event-level
   #:image-event-condition
   #:image-event-restarts
   #:image-event-frames
   #:image-event-job
   #:image-event-job-state
   #:image-event-text
   #:+image-event-frames+
   #:+image-event-tag+
   #:debugger-event-text
   #:lisp-tools-pending-events
   #:take-image-events
   #:image-event-turn
   #:image-event-turn-p
   #:post-image-events
   ;; conditions
   #:observe-mode-refused))

(in-package #:lisp-mcp/agent)
