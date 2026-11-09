(in-package #:lisp-mcp)

;;; Tool registry

(defvar *tools* '() "List of (name description schema annotations handler), newest first.")

(defmacro define-tool (name (args-var &key description (kind :read)) schema &body body)
  "Register tool NAME. KIND is :READ (readOnlyHint) or :WRITE (destructiveHint).
BODY runs with ARGS-VAR bound to the JSON arguments object and returns text."
  `(progn
     (setf *tools* (remove ,name *tools* :key #'first :test #'string=))
     (push (list ,name ,description ,schema
                 (mcp:json-object "readOnlyHint" ,(eq kind :read)
                                  "destructiveHint" ,(eq kind :write))
                 (lambda (,args-var) (declare (ignorable ,args-var)) ,@body))
           *tools*)
     ,name))

(defparameter +path+ '("path" "string" "File path, relative to the project root" :required t))
(defparameter +locator+
  '("locator" "string"
    "Form to act on: \"defun foo\", \"defmethod print-object (my-class t)\", \"defclass bar\", \"path:0.2.1\" (top-level index then child indices), \"line:12\" or \"line:12:4\", \"offset:120\", \"hash:<form hash>\""
    :required t))

(defun read-project-file (path)
  (let ((file (resolve-project-path path)))
    (values (cached-source file) (relative-name file))))

(defun locate-in (parsed locator)
  (handler-bind ((src:ambiguous-locator
                   (lambda (c)
                     (error 'bad-argument
                            :message (format nil "Locator ~S matches ~D forms; use one of:~{~%  ~A~}"
                                             (src:locator-error-locator c)
                                             (length (src:ambiguous-locator-candidates c))
                                             (mapcar #'path-string (src:ambiguous-locator-candidates c)))))))
    (src:locate parsed (parse-locator locator))))

(defun project-sources (&key (progress-message "Reading"))
  (call-with-progress (project-files) progress-message #'cached-source))

;;; Source tools (read only)

(defparameter +live+
  '("live" "boolean" "Also ask the connected image (default connection, or `connection`) and merge its answer, marked [image]"))
(defparameter +live-connection+ '("connection" "string" "Connection for live=true (default: the most recent)"))

(defun live-connection (args)
  "The connection to ask when live=true, or NIL with a note when there is none."
  (when (arg-bool args "live")
    (handler-case (find-connection (arg-string args "connection"))
      ((or no-connection unknown-connection) () nil))))

(defun image-defined-flags (conn parsed entries)
  "For each outline entry, T / NIL whether the image defines its name (as a
function, variable or class), or :UNKNOWN. One round trip; only FIND-SYMBOL is
used, so nothing is interned."
  (let* ((probes (loop for e in entries
                       for name = (src:outline-entry-name e)
                       for kind = (src:outline-entry-kind e)
                       collect (cond ((not (and (stringp name) (not (find #\Space name)))) nil)
                                     ((member kind '("in-package") :test #'string-equal) nil)
                                     ((member kind '("defpackage" "define-package") :test #'string-equal)
                                      (list :package (string-upcase name)))
                                     (t (list :symbol (string-upcase name)
                                              (src:package-at parsed (src:node-start (src:outline-entry-node e))))))))
         (form (format nil "(cl:list ~{~A~^ ~})"
                       (loop for p in probes
                             collect (case (first p)
                                       (:package (format nil "(cl:if (cl:find-package ~S) 1 0)" (second p)))
                                       (:symbol
                                        (format nil "(cl:let* ((p (cl:find-package ~S)) (s (cl:and p (cl:find-symbol ~S p)))) (cl:if (cl:and s (cl:or (cl:fboundp s) (cl:boundp s) (cl:find-class s cl:nil))) 1 0))"
                                                (third p) (second p)))
                                       (t "2")))))
         (result (repl:repl-eval conn form)))
    (if (and (typep result 'repl:eval-result) (eval-result-values-p result))
        (let ((digits (remove-if-not #'digit-char-p (first (repl:eval-result-values result)))))
          (loop for i from 0 below (length entries)
                collect (if (< i (length digits))
                            (case (char digits i) (#\1 t) (#\0 nil) (t :unknown))
                            :unknown)))
        (make-list (length entries) :initial-element :unknown))))

(defun eval-result-values-p (result)
  (and (not (repl:eval-result-aborted-p result)) (repl:eval-result-values result)))

(define-tool "source-outline"
    (args :description "Top-level definitions of a file: kind, name, lambda list, docstring, line and structural path. Start here before reading or editing. live=true marks which definitions the image has loaded.")
    (input-schema +path+ +live+ +live-connection+)
  (multiple-value-bind (parsed name) (read-project-file (require-arg args "path"))
    (let ((conn (live-connection args)))
      (cond (conn
             (let ((entries (src:outline parsed)))
               (render-outline parsed name :marks (image-defined-flags conn parsed entries))))
            ((arg-bool args "live")
             (concatenate 'string (render-outline parsed name) (format nil "(live: no connection)~%")))
            (t (render-outline parsed name))))))

(define-tool "source-read"
    (args :description "Text of one form (by locator) with its structural path, line range and the file hash to pass as expected_hash to edit tools. Without a locator, returns the outline.")
    (input-schema +path+ (append (butlast +locator+ 2) '(:required nil)))
  (multiple-value-bind (parsed name) (read-project-file (require-arg args "path"))
    (let ((locator (arg-string args "locator")))
      (if locator
          (render-node parsed name (locate-in parsed locator))
          (render-outline parsed name)))))

(define-tool "source-check"
    (args :description "Read-check and lint a file without evaluating it: reader errors, paren imbalances with suggested fix positions, unused variables, undefined exports, missing docstrings, missing ASDF components, #. forms.")
    (input-schema +path+
                  '("rules" "string" "Comma-separated lint rules (default all): unused-variable, missing-in-package, undefined-export, missing-docstring, asd-missing-component, none"))
  (multiple-value-bind (parsed name) (read-project-file (require-arg args "path"))
    (let* ((rules-arg (arg-string args "rules"))
           (rules (cond ((null rules-arg) src:*lint-rules*)
                        ((string-equal rules-arg "none") '())
                        (t (loop for r in (uiop:split-string rules-arg :separator ",")
                                 for name = (string-trim " " r)
                                 for rule = (find name src:*lint-rules* :key #'symbol-name :test #'string-equal)
                                 unless rule do (error 'bad-argument :message (format nil "Unknown rule ~A" name))
                                 collect rule))))
           (model (and rules (project-model (project-sources)))))
      (render-diagnostics parsed name :lint (and rules (src:lint parsed :rules rules :model model))))))

(defun symbol-args (args)
  (let ((symbol (require-arg args "symbol")))
    (let ((colon (position #\: symbol)))
      (if colon
          (values (subseq symbol 0 colon) (string-left-trim ":" (subseq symbol colon)))
          (values (or (arg-string args "package") "COMMON-LISP-USER") symbol)))))

(defparameter +symbol+ '("symbol" "string" "Symbol, optionally package-qualified (pkg:name)" :required t))
(defparameter +package+ '("package" "string" "Package for an unqualified symbol"))

(defun live-section (args title function)
  "Text for a live=true section: FUNCTION's result from the connection, or a note."
  (if (not (arg-bool args "live"))
      ""
      (let ((conn (live-connection args)))
        (if conn
            (format nil "[image] ~A:~%~A" title (funcall function conn))
            (format nil "[image] no connection~%")))))

(defun qualified (package name) (format nil "~A::~A" package name))

(defun display-symbol (model package name)
  "PACKAGE:NAME when the package exports NAME, else PACKAGE::NAME."
  (let ((package (src:canonical-package-name model (string-upcase package)))
        (name (string-upcase name)))
    (format nil "~A~:[::~;:~]~A" package (src:package-exports-p model package name) name)))

(define-tool "source-find-definitions"
    (args :description "Static definitions of a symbol across the project (.lisp/.asd), resolved through defpackage :use / :import-from / nicknames without loading code. live=true adds the image's own definition locations (including generated or macro-made ones)."
          )
    (input-schema +symbol+ +package+ +live+ +live-connection+)
  (multiple-value-bind (package name) (symbol-args args)
    (let* ((sources (project-sources))
           (model (project-model sources))
           (defs (src:find-definitions model package name :sources sources)))
      (concatenate
       'string
       (if defs
           (with-text (s)
             (loop for (ps . entry) in defs
                   for node = (src:outline-entry-node entry)
                   do (format s "~A:~A ~A ~A [~A]~%" (relative-name (src:parsed-source-path ps))
                              (position-string ps (src:node-start node))
                              (src:outline-entry-kind entry) (src:outline-entry-name entry) (path-string node))))
           (format nil "No static definition of ~A in ~D files~%" (display-symbol model package name) (length sources)))
       (live-section args "definitions"
                     (lambda (conn) (render-xrefs (repl:repl-find-definitions conn (qualified package name)))))))))

(define-tool "source-find-references"
    (args :description "Static references to a symbol across the project, including defpackage :export / :import-from designators. live=true adds the image's who-calls and who-references (sees through macros).")
    (input-schema +symbol+ +package+ +live+ +live-connection+)
  (multiple-value-bind (package name) (symbol-args args)
    (let* ((sources (project-sources))
           (model (project-model sources))
           (refs (src:find-references model package name :sources sources)))
      (concatenate
       'string
       (with-text (s)
         (format s "~D reference~:P to ~A~%" (length refs) (display-symbol model package name))
         (loop for (ps . node) in refs
               do (format s "  ~A:~A ~A~%" (relative-name (src:parsed-source-path ps))
                          (position-string ps (src:node-start node)) (src:node-text node))))
       (live-section args "who-calls"
                     (lambda (conn) (render-xrefs (repl:repl-xref conn :calls (qualified package name)))))
       (live-section args "who-references"
                     (lambda (conn) (render-xrefs (repl:repl-xref conn :references (qualified package name)))))))))

(define-tool "source-packages"
    (args :description "Packages declared in the project (defpackage / define-package): nicknames, used packages, export count, local nicknames and where each is defined.")
    (input-schema)
  (let* ((sources (project-sources))
         (packages (src:package-model-packages (project-model sources))))
    (if (null packages)
        (format nil "No package definitions in ~D files~%" (length sources))
        (with-text (s)
          (dolist (p packages)
            (let ((ps (src:package-info-source p)) (node (src:package-info-node p)))
              (format s "~A~@[ (nicknames ~{~A~^ ~})~]~@[  ~A~]~%  use ~{~A~^ ~}; ~D export~:P~@[; local nicknames ~{~A~^ ~}~]~%"
                      (src:package-info-name p) (src:package-info-nicknames p)
                      (and ps node (format nil "~A:~A" (relative-name (src:parsed-source-path ps))
                                           (position-string ps (src:node-start node))))
                      (src:package-info-use p) (length (src:package-info-exports p))
                      (loop for (nick . target) in (src:package-info-local-nicknames p)
                            collect (format nil "~A=~A" nick target)))))))))
