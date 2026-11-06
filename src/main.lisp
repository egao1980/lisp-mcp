(in-package #:lisp-mcp)

(defun parse-cli (args)
  "Plist from --key value / --flag arguments."
  (loop with out = '()
        while args
        do (let ((a (pop args)))
             (unless (and (> (length a) 2) (string= "--" a :end2 2))
               (error 'bad-argument :message (format nil "Unexpected argument ~A" a)))
             (let ((key (intern (string-upcase (subseq a 2)) :keyword)))
               (push key out)
               (push (if (and args (not (string= "--" (first args) :end2 (min 2 (length (first args))))))
                         (pop args)
                         t)
                     out)))
        finally (return (nreverse out))))

(defun main (&key (args (uiop:command-line-arguments)))
  "Entry point: lisp-mcp [--transport stdio|http] [--root DIR] [--host H] [--port P]
[--spawn swank|slynk] [--attach HOST:PORT] [--backend swank|slynk]."
  (let* ((opts (parse-cli args))
         (root (or (getf opts :root) (uiop:getenv "MCP_PROJECT_ROOT") (uiop:getcwd)))
         (server (make-lisp-mcp-server :root root)))
    (alexandria:when-let ((backend (getf opts :spawn)))
      (let ((c (repl:repl-spawn (backend-named backend) :directory (state-root *state*))))
        (add-connection (fresh-connection-name backend) c)))
    (alexandria:when-let ((attach (getf opts :attach)))
      (let* ((colon (position #\: attach :from-end t))
             (c (repl:repl-connect (backend-named (getf opts :backend))
                                   :host (if colon (subseq attach 0 colon) "127.0.0.1")
                                   :port (parse-integer attach :start (if colon (1+ colon) 0)))))
        (add-connection "attached-1" c)))
    (if (string-equal (getf opts :transport "stdio") "http")
        (progn
          (unless (find-package "MCP-BACKEND-STREAMABLE-HTTP")
            (asdf:load-system "lisp-mcp/http"))
          (uiop:symbol-call "MCP-BACKEND-STREAMABLE-HTTP" "USE-STREAMABLE-HTTP-MCP-BACKEND")
          (mcp:mcp-serve server :host (getf opts :host "127.0.0.1")
                                :port (parse-integer (getf opts :port "3000"))))
        (progn
          (mcp-backend-stdio:use-stdio-mcp-backend)
          (mcp:mcp-serve server)))))
