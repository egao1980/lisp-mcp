(in-package #:lisp-mcp)

(defvar *verbose* nil
  "True while a tool call asked for verbose output (argument verbose=true), or
when the server was started with --verbose / LISP_MCP_VERBOSE=1. Terse output
drops hints, long backtraces, diffs of written edits and passing test lines.")

(defvar *default-verbose* nil
  "Server-wide default for *VERBOSE*.")


;;; Server state: the project root (every path is resolved inside it) and
;;; the named connections to live images.

(define-condition lisp-mcp-error (error) ())

(define-condition path-outside-root (lisp-mcp-error)
  ((path :initarg :path :reader offending-path)
   (root :initarg :root :reader offending-root))
  (:report (lambda (c s)
             (format s "~A is outside the project root ~A" (offending-path c) (offending-root c)))))

(define-condition unknown-connection (lisp-mcp-error)
  ((name :initarg :name :reader connection-name))
  (:report (lambda (c s)
             (format s "No connection named ~S; use repl-list, repl-connect or repl-spawn"
                     (connection-name c)))))

(define-condition no-connection (lisp-mcp-error) ()
  (:report "No live image: call repl-spawn (fresh SBCL) or repl-connect (running Swank/Slynk server) first"))

(define-condition bad-argument (lisp-mcp-error)
  ((message :initarg :message :reader bad-argument-message))
  (:report (lambda (c s) (write-string (bad-argument-message c) s))))

;;; Tool groups: a server can offer a subset, so one binary runs as a
;;; static code server and a live image server.

(defparameter *tool-groups* '(:source :edit :output :repl :debug :inspect :jobs)
  "Every tool group, in catalog order.")

(defparameter *image-groups* '(:repl :debug :inspect :jobs)
  "Groups that work on a live image; image resources, notifications and the
image arguments of source and edit tools come with them.")

(defparameter *tool-presets*
  '(("all" :source :edit :output :repl :debug :inspect :jobs)
    ("code" :source :edit :output)
    ("image" :repl :debug :inspect :jobs :output))
  "Names for common --tools selections.")

(defun tool-group (name)
  "Group of tool NAME, from its prefix."
  (loop for (prefix . group) in '(("source-" . :source) ("edit-" . :edit) ("output-" . :output)
                                  ("repl-" . :repl) ("debug-" . :debug) ("inspect" . :inspect)
                                  ("job-" . :jobs))
        when (alexandria:starts-with-subseq prefix name) return group
        finally (error "Tool ~A has no group prefix" name)))

(defun parse-tool-groups (spec)
  "Groups selected by SPEC: NIL (all), a list of group keywords, or a comma
list of group and preset names (\"code\", \"image\", \"all\", \"source,edit\")."
  (flet ((unknown (what)
           (error 'bad-argument
                  :message (format nil "Unknown tool group ~S; use ~{~(~A~)~^, ~} or ~{~A~^, ~}"
                                   what *tool-groups* (mapcar #'first *tool-presets*)))))
    (let ((groups
            (cond ((null spec) *tool-groups*)
                  ((listp spec)
                   (dolist (g spec spec) (unless (member g *tool-groups*) (unknown g))))
                  (t (loop for part in (uiop:split-string spec :separator ",")
                           for name = (string-trim " " part)
                           unless (string= name "")
                             append (or (rest (assoc name *tool-presets* :test #'string-equal))
                                        (let ((g (find name *tool-groups* :test #'string-equal)))
                                          (if g (list g) (unknown name)))))))))
      (when (null groups) (unknown spec))
      ;; catalog order, no duplicates
      (remove-if-not (lambda (g) (member g groups)) *tool-groups*))))

(defclass lisp-mcp-state ()
  ((root :initarg :root :accessor state-root
         :documentation "Directory pathname every tool path is resolved against.")
   (tool-groups :initarg :tool-groups :initform *tool-groups* :reader state-tool-groups
                :documentation "Tool groups this server offers (see *TOOL-GROUPS*).")
   (connections :initform (make-hash-table :test #'equal) :reader state-connections)
   (default :initform nil :accessor state-default-connection)
   (channels :initform (make-hash-table :test #'equal) :reader state-channels
             :documentation "Channel name → REPL-CHANNEL.")
   (image-indentation :initform '() :accessor state-image-indentation
                      :documentation "(name . spec) hints sent by connected images.")
   (source-cache :initform nil :accessor state-source-cache)
   (outputs :initform '() :accessor state-outputs
            :documentation "STORED-OUTPUT objects, newest first.")
   (output-counter :initform 0 :accessor state-output-counter)
   (jobs :initform '() :accessor state-jobs
         :documentation "JOB objects returned to the client as running, newest first.")
   (job-counter :initform 0 :accessor state-job-counter)
   (job-cv :initform (bt2:make-condition-variable :name "lisp-mcp jobs") :reader state-job-cv)
   (connection-hook :initform nil :accessor state-connection-hook
                    :documentation "NIL, or a function (NAME KIND DATA) CONNECTION-LISTENER
calls for every event of connection NAME, after the MCP notifications. Read when
the event arrives. Set by LISP-MCP/AGENT:MAKE-LISP-TOOLS (H4b image events).")
   (job-hook :initform nil :accessor state-job-hook
             :documentation "NIL, or a function (JOB) FINISH-JOB calls when a detached JOB
has ended. Set by LISP-MCP/AGENT:MAKE-LISP-TOOLS (H4b image events).")
   (lock :initform (bt2:make-lock :name "lisp-mcp state") :reader state-lock)))

(defvar *state* nil "The LISP-MCP-STATE of the running server.")

(defun make-state (&key (root (uiop:getcwd)) tool-groups)
  (make-instance 'lisp-mcp-state :root (uiop:ensure-directory-pathname (truename root))
                                 :tool-groups (parse-tool-groups tool-groups)))

(defun group-on-p (group &key (state *state*))
  (and (member group (state-tool-groups state)) t))

(defun image-tools-p (&key (state *state*))
  "True when any live image group is on."
  (and (intersection *image-groups* (state-tool-groups state)) t))

(defun resolve-project-path (path &key (state *state*) (must-exist t))
  "Absolute pathname for PATH (relative to the root, or absolute inside it).
Containment is checked before existence, and again on the truename so a
symlink cannot point outside the root."
  (let* ((root (state-root state))
         (root-name (namestring root))
         (full (uiop:merge-pathnames* (uiop:parse-native-namestring path) root)))
    (flet ((inside-p (p)
             (let ((name (namestring p)))
               (and (>= (length name) (length root-name))
                    (string= root-name name :end2 (length root-name))
                    (not (member :up (pathname-directory p)))
                    (not (member :back (pathname-directory p)))
                    (not (search "/../" name))))))
      (unless (inside-p full)
        (error 'path-outside-root :path path :root root-name))
      (let ((true (probe-file full)))
        (cond (true
               (unless (inside-p true) (error 'path-outside-root :path path :root root-name))
               true)
              (must-exist (error 'bad-argument :message (format nil "No such file: ~A" path)))
              (t full))))))

(defun relative-name (pathname &key (state *state*))
  (let ((root (namestring (state-root state)))
        (name (namestring pathname)))
    (if (and (>= (length name) (length root)) (string= root name :end2 (length root)))
        (subseq name (length root))
        name)))

(defun hidden-path-p (path)
  (some (lambda (part) (and (stringp part) (plusp (length part)) (char= (char part 0) #\.)))
        (rest (pathname-directory path))))

(defun project-files (&key (state *state*))
  "Every .lisp / .asd file under the root, hidden directories skipped."
  (let ((root (state-root state)))
    (sort (remove-if #'hidden-path-p
                     (append (directory (merge-pathnames "**/*.lisp" root) :resolve-symlinks nil)
                             (directory (merge-pathnames "**/*.asd" root) :resolve-symlinks nil)))
          #'string< :key #'namestring)))

;;; Connections

(defun add-connection (name conn &key (state *state*))
  "Register CONN as NAME (now the default) and route its events to the client."
  (bt2:with-lock-held ((state-lock state))
    (setf (gethash name (state-connections state)) conn
          (state-default-connection state) name))
  (watch-connection name conn)
  conn)

(defun remove-connection (name &key (state *state*))
  (bt2:with-lock-held ((state-lock state))
    (let ((conn (gethash name (state-connections state))))
      ;; channels die with their connection
      (loop for channel-name in (loop for k being the hash-keys of (state-channels state)
                                        using (hash-value ch)
                                      when (eq (repl:repl-channel-connection ch) conn) collect k)
            do (remhash channel-name (state-channels state))))
    (remhash name (state-connections state))
    (when (equal (state-default-connection state) name)
      (setf (state-default-connection state)
            (loop for k being the hash-keys of (state-connections state) return k)))))

(defun find-connection (&optional name (state *state*))
  "Connection NAME, or the default one."
  (bt2:with-lock-held ((state-lock state))
    (let ((name (or name (state-default-connection state))))
      (cond ((null name) (error 'no-connection))
            ((gethash name (state-connections state)))
            (t (error 'unknown-connection :name name))))))

(defun find-channel (name &key (state *state*))
  (bt2:with-lock-held ((state-lock state))
    (or (gethash name (state-channels state))
        (error 'bad-argument :message (format nil "No channel named ~S; use repl-channel-open" name)))))

(defun fresh-channel-name (&key (state *state*))
  (loop for i from 1
        for name = (format nil "channel-~D" i)
        unless (gethash name (state-channels state)) return name))

(defun fresh-connection-name (prefix &key (state *state*))
  (loop for i from 1
        for name = (format nil "~A-~D" prefix i)
        unless (gethash name (state-connections state)) return name))
