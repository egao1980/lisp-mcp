(in-package #:lisp-mcp/tests)

;;; The server end to end over its two transports, in process.

(defun request (id method params)
  (rpc-protocol:encode-request method params :id id))

(defun legacy-initialize (id)
  (request id "initialize" (mcp:json-object "protocolVersion" "2025-11-25"
                                            "capabilities" (mcp:json-object)
                                            "clientInfo" (mcp:json-object "name" "test" "version" "0"))))

(deftest stdio-end-to-end
  (with-project (root)
    (write-project-file root "src/demo.lisp" *demo*)
    (let* ((input (format nil "~{~A~%~}"
                          (list (legacy-initialize 1)
                                (rpc-protocol:encode-notification "notifications/initialized" (mcp:json-object))
                                (request 2 "tools/list" (mcp:json-object))
                                (request 3 "tools/call" (mcp:json-object
                                                         "name" "source-find-references"
                                                         "arguments" (mcp:json-object "symbol" "demo:frob")
                                                         "_meta" (mcp:json-object "progressToken" 9)))
                                (request 4 "resources/templates/list" (mcp:json-object)))))
           (out (make-string-output-stream)))
      (mcp:backend-mcp-serve (mcp-backend-stdio:make-stdio-mcp-backend) *server*
                             :input (make-string-input-stream input) :output out)
      (let* ((messages (with-input-from-string (s (get-output-stream-string out))
                         (loop for line = (read-line s nil) while line
                               collect (rpc-protocol:decode-message line))))
             (by-id (lambda (id) (find id messages :key (lambda (m) (gethash "id" m))))))
        (ok (equal "lisp-mcp" (gethash "name" (gethash "serverInfo" (gethash "result" (funcall by-id 1))))))
        (let ((tools (gethash "tools" (gethash "result" (funcall by-id 2)))))
          (ok (>= (length tools) 55))
          (ok (find "edit-move" tools :key (lambda (h) (gethash "name" h)) :test #'equal)))
        (ok (search "references to DEMO:FROB"
                    (gethash "text" (elt (gethash "content" (gethash "result" (funcall by-id 3))) 0))))
        (ok (find-if (lambda (m) (and (equal (gethash "method" m) "notifications/progress")
                                      (eql 9 (gethash "progressToken" (gethash "params" m)))))
                     messages)
            "progress notifications reach the client")
        (ok (plusp (length (gethash "resourceTemplates" (gethash "result" (funcall by-id 4))))))))))

(defun http-headers (method name)
  (let ((h (make-hash-table :test #'equal)))
    (setf (gethash "accept" h) "application/json, text/event-stream"
          (gethash "mcp-protocol-version" h) "2025-11-25"
          (gethash "mcp-method" h) method)
    (when name (setf (gethash "mcp-name" h) name))
    h))

(defun http-call (app id method params &optional name)
  "POST one request to APP; → list of decoded SSE message payloads."
  (let ((response (funcall app (list :request-method :post :path-info "/"
                                     :raw-body (request id method params)
                                     :headers (http-headers method name)))))
    (values (mapcar (lambda (chunk)
                      (rpc-protocol:decode-message
                       (sse-protocol:sse-event-data (sse-protocol:decode-sse-block chunk))))
                    (third response))
            (first response))))

(deftest http-end-to-end
  (with-project (root)
    (write-project-file root "src/demo.lisp" *demo*)
    (let ((app (mcp-backend-streamable-http:make-mcp-app *server*)))
      (multiple-value-bind (messages status) (http-call app 1 "tools/list" (mcp:json-object))
        (ok (eql 200 status))
        (ok (>= (length (gethash "tools" (gethash "result" (first messages)))) 55)))
      (let ((messages (http-call app 2 "tools/call"
                                 (mcp:json-object "name" "source-outline"
                                                  "arguments" (mcp:json-object "path" "src/demo.lisp"))
                                 "source-outline")))
        (ok (search "defun frob (x)"
                    (gethash "text" (elt (gethash "content" (gethash "result" (car (last messages)))) 0)))))
      ;; progress travels in the same SSE response, before the result
      (let ((messages (http-call app 3 "tools/call"
                                 (mcp:json-object "name" "source-find-references"
                                                  "arguments" (mcp:json-object "symbol" "demo:frob")
                                                  "_meta" (mcp:json-object "progressToken" "h"))
                                 "source-find-references")))
        (ok (equal "notifications/progress" (gethash "method" (first messages))))
        (ok (eql 3 (gethash "id" (car (last messages))))))
      (let ((messages (http-call app 4 "resources/read"
                                 (mcp:json-object "uri" "lisp-source:///src/demo.lisp")
                                 "lisp-source:///src/demo.lisp")))
        (ok (search "defun caller"
                    (gethash "text" (elt (gethash "contents" (gethash "result" (first messages))) 0))))))))
