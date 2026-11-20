(in-package #:lisp-mcp)

;;; FORM-RESOURCES (H4a, pure): a conservative scanner from Lisp code to
;;; permission resources. It reads with lisp-source-protocol (Eclector CST:
;;; never evaluates, never interns; #. becomes a placeholder) and walks the
;;; forms without macroexpansion: special forms and a known list of standard
;;; macros and functions are modelled, any other operator is named as
;;; "lisp:calls <pkg>:<name>", and what cannot be attributed to a name is
;;; "lisp:*". The grammar and the tables are in the H4a brief
;;; (cl-stack docs/capabilities/lisp-agent-tools.md).
;;;
;;; Contract stub: the implementation removes %NOT-IMPLEMENTED and every call.

(defun %not-implemented (name)
  (error "~A is not implemented (lisp-mcp H4a contract)" name))

(defun form-resources (code &key (package "cl-user") features)
  "→ permission resource strings for evaluating CODE (a string of zero or
more forms) in PACKAGE: \"lisp:pure\" alone, or \"lisp:reads <sym>\",
\"lisp:defines <sym>\", \"lisp:mutates <sym>\", \"lisp:package <name>\",
\"lisp:io\", \"lisp:read-eval\", \"lisp:calls <sym>\" in order of appearance
without duplicates, then \"lisp:*\" when anything was not modelled. NIL for
blank or comment-only CODE. FEATURES (keywords) decide #+/#-; NIL means the
host *FEATURES*. Never signals."
  (declare (ignore code package features))
  (%not-implemented 'form-resources))
