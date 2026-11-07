(in-package #:lisp-mcp)

;;; Parsed sources are cached by file: a file is re-read only when its write
;;; date or length changed. The package model and the indentation learned
;;; from project macros are rebuilt only when some source changed.

(defclass source-cache ()
  ((entries :initform (make-hash-table :test #'equal) :reader cache-entries
            :documentation "Namestring → (write-date length parsed-source).")
   (generation :initform 0 :accessor cache-generation
               :documentation "Incremented whenever a cached source is replaced.")
   (model :initform nil :accessor cache-model
          :documentation "(generation file-names package-model) for the last project model.")
   (indentation-generation :initform -1 :accessor cache-indentation-generation)
   (lock :initform (bt2:make-lock :name "lisp-mcp source cache") :reader cache-lock)))

(defun state-cache (&optional (state *state*))
  (or (state-source-cache state)
      (setf (state-source-cache state) (make-instance 'source-cache))))

(defun cached-source (pathname &key (state *state*))
  "Parsed source of PATHNAME, re-read only when the file changed."
  (let* ((cache (state-cache state))
         (key (namestring pathname))
         (date (file-write-date pathname))
         (length (with-open-file (in pathname :element-type '(unsigned-byte 8)) (file-length in))))
    (bt2:with-lock-held ((cache-lock cache))
      (destructuring-bind (&optional old-date old-length parsed) (gethash key (cache-entries cache))
        (if (and parsed (eql old-date date) (eql old-length length))
            parsed
            (let ((fresh (src:read-source-file pathname)))
              (setf (gethash key (cache-entries cache)) (list date length fresh))
              (incf (cache-generation cache))
              fresh))))))

(defun forget-source (pathname &key (state *state*))
  "Drop PATHNAME from the cache (after this server wrote it)."
  (let ((cache (state-cache state)))
    (bt2:with-lock-held ((cache-lock cache))
      (when (remhash (namestring pathname) (cache-entries cache))
        (incf (cache-generation cache))))))

(defun project-model (sources &key (state *state*))
  "Package model of SOURCES, reused while no source changed."
  (let* ((cache (state-cache state))
         (names (mapcar (lambda (ps) (namestring (src:parsed-source-path ps))) sources)))
    (bt2:with-lock-held ((cache-lock cache))
      (destructuring-bind (&optional generation old-names model) (cache-model cache)
        (if (and model (eql generation (cache-generation cache)) (equal names old-names))
            model
            (let ((fresh (src:build-package-model sources)))
              (setf (cache-model cache) (list (cache-generation cache) names fresh))
              fresh))))))
