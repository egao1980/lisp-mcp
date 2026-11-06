(defpackage #:lisp-mcp/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:mcp #:mcp-protocol)))

(in-package #:lisp-mcp/tests)

(defvar *server* nil)

(defun call (tool &rest kvs)
  "Call TOOL; → (values text error-p)."
  (let* ((result (mcp:call-tool *server* tool (apply #'mcp:json-object kvs)))
         (content (gethash "content" result)))
    (values (format nil "~{~A~}" (map 'list (lambda (c) (gethash "text" c)) content))
            (gethash "isError" result))))

(defun write-project-file (root name text)
  (let ((path (merge-pathnames name root)))
    (ensure-directories-exist path)
    (alexandria:write-string-into-file text path :if-exists :supersede)
    path))

(defmacro with-project ((root) &body body)
  `(let* ((,root (uiop:ensure-directory-pathname
                  (merge-pathnames (format nil "lisp-mcp-test-~D/" (random 1000000 (make-random-state t)))
                                   (uiop:temporary-directory))))
          (*server* (progn (ensure-directories-exist ,root)
                           (lisp-mcp:make-lisp-mcp-server :root ,root))))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,root :validate t :if-does-not-exist :ignore))))
