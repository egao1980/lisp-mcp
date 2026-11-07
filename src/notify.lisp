(in-package #:lisp-mcp)

;;; Notifications to the MCP client: log messages, progress, and resource
;;; updates driven by events from connected images.

(defvar *server* nil "The MCP-SERVER being served.")

(defun notify-log (level logger control &rest args)
  "Send an MCP log message (filtered by the client's logging level)."
  (when *server*
    (mcp:mcp-log *server* level (apply #'format nil control args) :logger logger)))

(defun progress (done total control &rest args)
  "Report progress of the tool call being handled (sent only when the client
asked for it with a progressToken)."
  (when *server*
    (mcp:send-progress *server* done :total total :message (apply #'format nil control args))))

(defun call-with-progress (items message function)
  "Call FUNCTION on each of ITEMS, reporting progress about every 5 %."
  (let* ((total (length items))
         (step (max 1 (floor total 20))))
    (loop for item in items
          for i from 1
          collect (prog1 (funcall function item)
                    (when (or (= i total) (zerop (mod i step)))
                      (progress i total "~A ~D/~D" message i total))))))

(defun output-uri (name) (format nil "repl://~A/output" name))
(defun debugger-uri (name) (format nil "repl://~A/debugger" name))

(defun wire-indentation-spec (spec)
  "Distinguished-argument count from a Swank/Slynk indentation spec: an integer,
or a list whose &BODY marks the body."
  (cond ((integerp spec) spec)
        ((listp spec)
         (position-if (lambda (x)
                        (equal (if (repl:wire-symbol-p x) (repl:wire-symbol-name x) (and (symbolp x) (symbol-name x)))
                               "&BODY"))
                      spec))
        (t nil)))

(defun record-image-indentation (entries)
  "Keep the image's indentation hints: (name spec [packages]) or (name . spec)."
  (bt2:with-lock-held ((state-lock *state*))
    (dolist (entry entries)
      (when (and (consp entry) (stringp (first entry)))
        (let ((spec (wire-indentation-spec (if (consp (rest entry)) (second entry) (rest entry)))))
          (when spec
            (push (cons (first entry) spec) (state-image-indentation *state*)))))))
  ;; re-apply on the next edit
  (setf (cache-indentation-generation (state-cache)) -1))

(defun connection-listener (name)
  "Event listener for connection NAME: turns image events into MCP notifications."
  (let ((server *server*) (state *state*))
    (lambda (kind data)
      (let ((*server* server) (*state* state))
        (case kind
          (:output
           (mcp:notify-resources-updated server (output-uri name))
           (notify-log "info" (format nil "repl/~A" name) "~A" data))
          (:debug
           (mcp:notify-resources-updated server (debugger-uri name))
           (notify-log "warning" (format nil "repl/~A" name)
                       "~A entered the debugger (level ~A): ~A~%Use debug-state, debug-backtrace, debug-invoke-restart."
                       name (repl:debugger-state-level data) (repl:debugger-state-condition data)))
          (:debug-return
           (mcp:notify-resources-updated server (debugger-uri name)))
          (:indentation
           (record-image-indentation data)))))))

(defun watch-connection (name conn)
  (setf (repl:repl-event-listener conn) (connection-listener name))
  conn)

;;; Indentation for edits: the standard table, the image's hints, then the
;;; project's own macros.

(defun prepare-indentation (sources)
  (let ((cache (state-cache)))
    (unless (eql (cache-indentation-generation cache) (cache-generation cache))
      (src:reset-indentation)
      (loop for (name . spec) in (reverse (state-image-indentation *state*))
            do (src:set-indentation name spec))
      (src:learn-indentation sources)
      (setf (cache-indentation-generation cache) (cache-generation cache)))))
