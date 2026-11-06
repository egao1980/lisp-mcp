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
    (values (src:read-source-file file) (relative-name file))))

(defun locate-in (parsed locator)
  (handler-bind ((src:ambiguous-locator
                   (lambda (c)
                     (error 'bad-argument
                            :message (format nil "Locator ~S matches ~D forms; use one of:~{~%  ~A~}"
                                             (src:locator-error-locator c)
                                             (length (src:ambiguous-locator-candidates c))
                                             (mapcar #'path-string (src:ambiguous-locator-candidates c)))))))
    (src:locate parsed (parse-locator locator))))

(defun project-sources ()
  (mapcar #'src:read-source-file (project-files)))

;;; Source tools (read only)

(define-tool "source-outline"
    (args :description "Top-level definitions of a file: kind, name, lambda list, docstring, line and structural path. Start here before reading or editing.")
    (input-schema +path+)
  (multiple-value-bind (parsed name) (read-project-file (require-arg args "path"))
    (render-outline parsed name)))

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
           (model (and rules (src:build-package-model (project-sources)))))
      (render-diagnostics parsed name :lint (and rules (src:lint parsed :rules rules :model model))))))

(defun symbol-args (args)
  (let ((symbol (require-arg args "symbol")))
    (let ((colon (position #\: symbol)))
      (if colon
          (values (subseq symbol 0 colon) (string-left-trim ":" (subseq symbol colon)))
          (values (or (arg-string args "package") "COMMON-LISP-USER") symbol)))))

(defparameter +symbol+ '("symbol" "string" "Symbol, optionally package-qualified (pkg:name)" :required t))
(defparameter +package+ '("package" "string" "Package for an unqualified symbol"))

(define-tool "source-find-definitions"
    (args :description "Static definitions of a symbol across the project (.lisp/.asd), resolved through defpackage :use / :import-from / nicknames without loading code.")
    (input-schema +symbol+ +package+)
  (multiple-value-bind (package name) (symbol-args args)
    (let* ((sources (project-sources))
           (model (src:build-package-model sources))
           (defs (src:find-definitions model package name :sources sources)))
      (if defs
          (with-text (s)
            (loop for (ps . entry) in defs
                  for node = (src:outline-entry-node entry)
                  do (format s "~A:~A ~A ~A [~A]~%" (relative-name (src:parsed-source-path ps))
                             (position-string ps (src:node-start node))
                             (src:outline-entry-kind entry) (src:outline-entry-name entry) (path-string node))))
          (format nil "No static definition of ~A::~A in ~D files~%" package name (length sources))))))

(define-tool "source-find-references"
    (args :description "Static references to a symbol across the project, including defpackage :export / :import-from designators.")
    (input-schema +symbol+ +package+)
  (multiple-value-bind (package name) (symbol-args args)
    (let* ((sources (project-sources))
           (model (src:build-package-model sources))
           (refs (src:find-references model package name :sources sources)))
      (with-text (s)
        (format s "~D reference~:P to ~A::~A~%" (length refs) (string-upcase package) (string-upcase name))
        (loop for (ps . node) in refs
              do (format s "  ~A:~A ~A~%" (relative-name (src:parsed-source-path ps))
                         (position-string ps (src:node-start node)) (src:node-text node)))))))
