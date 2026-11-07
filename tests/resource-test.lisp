(in-package #:lisp-mcp/tests)

(defun dispatch (method &rest kvs)
  (mcp:dispatch-mcp-method *server* method (apply #'mcp:json-object kvs)))

(defun resource-text (uri)
  (gethash "text" (elt (gethash "contents" (dispatch "resources/read" "uri" uri)) 0)))

(defmacro collecting-notifications ((var) &body body)
  "Run BODY with server notifications pushed onto VAR as (method . params), oldest first after."
  `(let ((,var '()))
     (setf (mcp:mcp-server-notifier *server*) (lambda (m p) (push (cons m p) ,var)))
     (unwind-protect (progn ,@body)
       (setf (mcp:mcp-server-notifier *server*) nil))))

(deftest resource-templates
  (with-project (root)
    (write-project-file root "src/demo.lisp" *demo*)
    (let ((templates (map 'list (lambda (h) (gethash "uriTemplate" h))
                          (gethash "resourceTemplates" (dispatch "resources/templates/list")))))
      (ok (member "lisp-source:///{+path}" templates :test #'string=))
      (ok (member "repl://{connection}/output" templates :test #'string=))
      (ok (member "repl://{connection}/debugger" templates :test #'string=)))
    (ok (search "defun frob (x)" (resource-text "lisp-source:///src/demo.lisp")))
    (ok (search "No connections" (resource-text "repl://connections")))
    (ok (signals (dispatch "resources/read" "uri" "lisp-source:///../x.lisp") 'mcp:mcp-error))
    (ok (signals (dispatch "resources/read" "uri" "repl://nope/output") 'mcp:mcp-error))
    ;; completion of the path variable
    (let ((values (gethash "values" (gethash "completion"
                                             (dispatch "completion/complete"
                                                       "ref" (mcp:json-object "type" "ref/resource"
                                                                              "uri" "lisp-source:///{+path}")
                                                       "argument" (mcp:json-object "name" "path" "value" "src/"))))))
      (ok (equalp values #("src/demo.lisp"))))))

(deftest progress-for-project-scans
  (with-project (root)
    (dotimes (i 30)
      (write-project-file root (format nil "src/f~D.lisp" i) (format nil "(defun f~D () (frob))" i)))
    (collecting-notifications (sent)
      (dispatch "tools/call" "name" "source-find-references"
                "arguments" (mcp:json-object "symbol" "frob")
                "_meta" (mcp:json-object "progressToken" "scan"))
      (let ((progress (remove "notifications/progress" sent :key #'car :test-not #'string=)))
        (ok (>= (length progress) 5))
        (ok (every (lambda (p) (equal "scan" (gethash "progressToken" (cdr p)))) progress))
        (ok (= 30 (gethash "progress" (cdr (first progress)))) "last report is complete")))
    ;; without a token nothing is sent
    (collecting-notifications (sent)
      (call "source-find-references" "symbol" "frob")
      (ok (null sent)))))
