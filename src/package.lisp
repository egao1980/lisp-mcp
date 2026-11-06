(defpackage #:lisp-mcp
  (:use #:cl)
  (:local-nicknames (#:src #:lisp-source-protocol)
                    (#:repl #:repl-protocol)
                    (#:mcp #:mcp-protocol))
  (:export #:make-lisp-mcp-server
           #:lisp-mcp-state
           #:*state*
           #:state-root
           #:resolve-project-path
           #:parse-locator
           #:path-outside-root
           #:unknown-connection
           #:no-connection
           #:main))
