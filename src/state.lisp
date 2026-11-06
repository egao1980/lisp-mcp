(in-package #:lisp-mcp)

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

(defclass lisp-mcp-state ()
  ((root :initarg :root :accessor state-root
         :documentation "Directory pathname every tool path is resolved against.")
   (connections :initform (make-hash-table :test #'equal) :reader state-connections)
   (default :initform nil :accessor state-default-connection)
   (lock :initform (bt2:make-lock :name "lisp-mcp state") :reader state-lock)))

(defvar *state* nil "The LISP-MCP-STATE of the running server.")

(defun make-state (&key (root (uiop:getcwd)))
  (make-instance 'lisp-mcp-state :root (uiop:ensure-directory-pathname (truename root))))

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
  (bt2:with-lock-held ((state-lock state))
    (setf (gethash name (state-connections state)) conn
          (state-default-connection state) name))
  conn)

(defun remove-connection (name &key (state *state*))
  (bt2:with-lock-held ((state-lock state))
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

(defun fresh-connection-name (prefix &key (state *state*))
  (loop for i from 1
        for name = (format nil "~A-~D" prefix i)
        unless (gethash name (state-connections state)) return name))
