(in-package #:lisp-mcp/tests)

;;; Tool groups (--tools / LISP_MCP_TOOLS) and what the catalog costs a client.

(defun stdio-catalog (tools)
  "Serve initialize + tools/list + resources/templates/list over stdio with
groups TOOLS. → (values instructions tool-names tools-list-chars templates)."
  (with-project (root)
    (let ((*server* (lisp-mcp:make-lisp-mcp-server :root root :tools tools))
          (out (make-string-output-stream)))
      (mcp:backend-mcp-serve (mcp-backend-stdio:make-stdio-mcp-backend) *server*
                             :input (make-string-input-stream
                                     (format nil "~{~A~%~}"
                                             (list (legacy-initialize 1)
                                                   (request 2 "tools/list" (mcp:json-object))
                                                   (request 3 "resources/templates/list" (mcp:json-object)))))
                             :output out)
      (let* ((lines (with-input-from-string (s (get-output-stream-string out))
                      (loop for line = (read-line s nil) while line collect line)))
             (messages (mapcar #'rpc-protocol:decode-message lines))
             (result (lambda (id) (gethash "result" (find id messages :key (lambda (m) (gethash "id" m))))))
             (list-line (find 2 lines :key (lambda (l) (gethash "id" (rpc-protocol:decode-message l))))))
        (values (gethash "instructions" (funcall result 1))
                (map 'list (lambda (h) (gethash "name" h)) (gethash "tools" (funcall result 2)))
                (length list-line)
                (map 'list (lambda (h) (gethash "uriTemplate" h))
                     (gethash "resourceTemplates" (funcall result 3))))))))

(defun names-with-prefix (names &rest prefixes)
  (remove-if-not (lambda (n) (some (lambda (p) (alexandria:starts-with-subseq p n)) prefixes)) names))

(deftest tool-group-parsing
  (ok (equal (lisp-mcp::parse-tool-groups nil) lisp-mcp::*tool-groups*))
  (ok (equal (lisp-mcp::parse-tool-groups "code") '(:source :edit :output)))
  (ok (equal (lisp-mcp::parse-tool-groups "image") '(:output :repl :debug :inspect :jobs)))
  (ok (equal (lisp-mcp::parse-tool-groups " edit , source,edit") '(:source :edit)) "trimmed, catalog order, no duplicates")
  (ok (equal (lisp-mcp::parse-tool-groups "code,jobs") '(:source :edit :output :jobs)))
  (ok (equal (lisp-mcp::parse-tool-groups '(:repl)) '(:repl)))
  (ok (signals (lisp-mcp::parse-tool-groups "source,bogus") 'lisp-mcp::bad-argument))
  (ok (signals (lisp-mcp::parse-tool-groups ",") 'lisp-mcp::bad-argument))
  (ok (signals (lisp-mcp::parse-tool-groups '(:nope)) 'lisp-mcp::bad-argument)))

(deftest code-preset-offers-only-static-tools
  (multiple-value-bind (instructions names bytes templates) (stdio-catalog "code")
    (ok (names-with-prefix names "source-"))
    (ok (member "edit-structure" names :test #'string=))
    (ok (member "output-read" names :test #'string=))
    (ok (null (names-with-prefix names "repl-" "debug-" "inspect" "job-")) "no image tools")
    (ok (search "Locator:" instructions))
    (ok (not (search "repl-spawn" instructions)) "instructions skip the image")
    (ok (not (search "compile=true" instructions)))
    (ok (not (search "repl://" instructions)))
    (ok (member "lisp-source:///{+path}" templates :test #'string=))
    (ok (not (find-if (lambda (u) (alexandria:starts-with-subseq "repl://" u)) templates)))
    (ok (< bytes 6000) (format nil "code catalog ~D bytes" bytes))))

(deftest code-preset-drops-image-arguments
  (with-project (root)
    (let* ((*server* (lisp-mcp:make-lisp-mcp-server :root root :tools "code"))
           (props (lambda (name)
                    (gethash "properties"
                             (mcp:mcp-tool-input-schema
                              (find name (mcp:list-tools *server*) :key #'mcp:mcp-tool-name :test #'string=))))))
      (ok (not (gethash "compile" (funcall props "edit-replace"))))
      (ok (not (gethash "connection" (funcall props "edit-structure"))))
      (ok (not (gethash "live" (funcall props "source-outline"))))
      (ok (gethash "expected_hash" (funcall props "edit-replace"))))
    ;; the full server still has them, so the copy did not change the shared schema
    (let* ((*server* (lisp-mcp:make-lisp-mcp-server :root root))
           (replace (find "edit-replace" (mcp:list-tools *server*) :key #'mcp:mcp-tool-name :test #'string=)))
      (ok (gethash "compile" (gethash "properties" (mcp:mcp-tool-input-schema replace)))))))

(deftest image-preset-offers-only-image-tools
  (multiple-value-bind (instructions names bytes templates) (stdio-catalog "image")
    (ok (member "repl-eval" names :test #'string=))
    (ok (member "debug-backtrace" names :test #'string=))
    (ok (member "inspect" names :test #'string=))
    (ok (member "job-wait" names :test #'string=))
    (ok (member "output-read" names :test #'string=))
    (ok (null (names-with-prefix names "source-" "edit-")) "no static tools")
    (ok (search "repl-spawn" instructions))
    (ok (search "repl://connections" instructions))
    (ok (not (search "Locator:" instructions)) "no edit rules")
    (ok (not (search "lisp-source://" instructions)))
    (ok (member "repl://{connection}/debugger" templates :test #'string=))
    (ok (not (member "lisp-source:///{+path}" templates :test #'string=)))
    (ok (< bytes 13000) (format nil "image catalog ~D bytes" bytes))))

(deftest each-group-alone
  ;; size budget per group, in bytes of the tools/list response
  (loop for (group budget) in '(("source" 2000) ("edit" 3800) ("output" 600) ("repl" 6500)
                                ("debug" 3000) ("inspect" 2300) ("jobs" 1000))
        do (multiple-value-bind (instructions names bytes) (stdio-catalog group)
             (ok names)
             (ok (every (lambda (n) (eq (lisp-mcp::tool-group n)
                                        (first (lisp-mcp::parse-tool-groups group))))
                        names)
                 (format nil "~A has only its own tools" group))
             (ok (plusp (length instructions)))
             (ok (<= bytes budget) (format nil "~A catalog ~D bytes (budget ~D)" group bytes budget)))))

(deftest whole-catalog-budget
  (multiple-value-bind (instructions names bytes) (stdio-catalog nil)
    (ok (= (length names) 56) (format nil "~D tools" (length names)))
    ;; 0.4.1 sent 36.8 KB for 63 tools; half of that at most
    (ok (<= bytes 18400) (format nil "catalog ~D bytes" bytes))
    (ok (<= (length instructions) 1700) (format nil "instructions ~D chars" (length instructions)))
    (dolist (part '("SOURCE" "EDIT" "LIVE" "DEBUG" "INSPECT" "JOBS" "LONG OUTPUT" "Locator:" "compile=true" "live=true"))
      (ok (search part instructions) part))))
