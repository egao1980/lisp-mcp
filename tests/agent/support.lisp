(in-package #:lisp-mcp/agent/tests)

;;; Shared helpers (test code, complete).

(defun has (needle haystack)
  (and (stringp haystack) (search needle haystack) t))

(defun message (condition)
  (and condition (princ-to-string condition)))

(defmacro failure (form)
  "The ERROR or AI-AGENT-PROTOCOL:AGENT-CONDITION (AGENT-UNKNOWN-TOOL is not an
ERROR) FORM signals, or NIL when it returns."
  `(handler-case (progn ,form nil)
     (error (c) c)
     (a:agent-condition (c) c)))

(defun real-failure (condition)
  "CONDITION when it is not the contract stub's not-implemented error."
  (and condition
       (not (has "is not implemented" (message condition)))
       condition))

(defun args (&rest kv)
  "String-keyed hash table from alternating keys and values."
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kv by #'cddr do (setf (gethash k h) v))
    h))

(defun json-args (&rest kv)
  "JSON object string (what a model sends) from alternating keys and values."
  (json:encode (apply #'args kv)))

;;; Projects

(defun fresh-directory ()
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "lisp-mcp-agent-~36R" (random (expt 36 8) (make-random-state t)))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (truename dir)))

(defun write-project-file (root name text)
  (let ((path (merge-pathnames name root)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string text out))
    path))

(defun read-project-file (root name)
  (uiop:read-file-string (merge-pathnames name root) :external-format :utf-8))

(defun call-with-tools (keys files fn)
  (let ((root (fresh-directory)) (tools nil))
    (unwind-protect
         (progn
           (loop for (name text) in files do (write-project-file root name text))
           (setf tools (apply #'make-lisp-tools :root root keys))
           (funcall fn tools root))
      (when tools (ignore-errors (close-lisp-tools tools)))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(defmacro with-tools ((tools root &rest keys) files &body body)
  "TOOLS: a LISP-TOOLS over a fresh ROOT holding FILES ((name text) …), made
with KEYS and closed afterwards."
  `(call-with-tools (list ,@keys) ,files (lambda (,tools ,root)
                                          (declare (ignorable ,root))
                                          ,@body)))

(defun tool (tools name &rest kv)
  "CALL-LISP-TOOL with the arguments as a JSON object string."
  (call-lisp-tool tools name (apply #'json-args kv)))

(defun resources (tools name &rest kv)
  (a:tool-permission-resources tools name (apply #'json-args kv)))

(defun approval-p (tools name &rest kv)
  (a:tool-needs-approval-p tools name (apply #'json-args kv)))

(defparameter +demo+ (format nil "(defun half (x) (/ x 2))~%~%(defun quarter (x) (half (half x)))~%"))

;;; Scanner checks

(defun scan (code &rest keys)
  (apply #'form-resources code keys))

(defun well-formed-p (result)
  "RESULT is a list of strings without duplicates, \"lisp:*\" at most once and
last, \"lisp:pure\" only alone."
  (and (listp result)
       (every #'stringp result)
       (= (length result) (length (remove-duplicates result :test #'string=)))
       (let ((star (position "lisp:*" result :test #'string=)))
         (or (null star) (= star (1- (length result)))))
       (or (not (member "lisp:pure" result :test #'string=))
           (equal result '("lisp:pure")))))

(defun resources= (result expected)
  (and (well-formed-p result)
       (= (length result) (length expected))
       (null (set-exclusive-or result expected :test #'string=))))

(defun check-scan (cases &rest keys)
  "CASES: (code expected-resources). One OK per case; order is not compared
except that \"lisp:*\" is last."
  (dolist (c cases)
    (destructuring-bind (code expected) c
      (ok (resources= (apply #'scan code keys) expected)
          (format nil "~S → ~S" code expected)))))

(defun check-scan-includes (cases &rest keys)
  "CASES: (code resource …): the scan is well formed and has each resource."
  (dolist (c cases)
    (destructuring-bind (code &rest wanted) c
      (ok (let ((r (apply #'scan code keys)))
            (and (well-formed-p r)
                 (every (lambda (w) (member w r :test #'string=)) wanted)))
          (format nil "~S has ~S" code wanted)))))

;;; Agent runs against a scripted LLM (libuv loop)

(defmacro with-agent-loop (&body body)
  `(let* ((eb (event-backend-libuv:make-libuv-backend))
          (el (event-protocol:make-event-loop eb)))
     (event-protocol:with-event-backend (eb)
       (event-protocol:with-event-loop-var (el)
         ,@body))))

(defun scripted-backend (steps box)
  "STEPS: tool calls (name . json-arguments) or a string (final reply). BOX's
car collects the turns each generate saw, oldest first."
  (let ((n 0))
    (llm:make-mock-llm-backend
     :handler (lambda (b turns &key &allow-other-keys)
                (declare (ignore b))
                (setf (car box) (append (car box) (list (copy-list turns))))
                (let ((step (nth (min n (1- (length steps))) steps)))
                  (incf n)
                  (if (stringp step)
                      (llm:make-llm-response
                       :parts (list (llm:make-llm-text-part :text step))
                       :finish-reason :stop)
                      (llm:make-llm-response
                       :parts (list (llm:make-llm-tool-call-part
                                     :id (format nil "c~d" n)
                                     :name (car step) :arguments (cdr step)))
                       :finish-reason :tool-use)))))))

(defun tool-results (run)
  "Tool result parts of RUN's turns, oldest first."
  (loop for turn in (a:agent-run-turns run)
        append (remove-if-not #'llm:llm-tool-result-part-p (llm:llm-turn-parts turn))))

(defun result-content (part) (llm:llm-tool-result-part-content part))
(defun result-error-p (part) (llm:llm-tool-result-part-error-p part))
