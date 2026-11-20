(in-package #:lisp-mcp/agent)

(define-condition observe-mode-refused (agent:tool-permission-denied) ()
  (:documentation "A LISP-TOOLS in :OBSERVE mode refused the call: the tool is
not an observe tool, or its code scans to resources outside the observe set.
ACTION is the tool name; RESOURCES are the offending resources (the tool's
resources for a tool that is not an observe tool). A tool result, not an
abort.")
  (:default-initargs :decision :deny))
