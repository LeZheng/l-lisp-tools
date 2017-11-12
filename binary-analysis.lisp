(defpackage :com.skyline.owl.binary
  (:use :COMMON-LISP :com.skyline.owl.lmacro)
  (:export :define-binary-class
           :define-tagged-binary-class
           :define-binary-type
           :read-value
           :write-value
           :*in-progress-objects*
           :parent-of-type
           :current-binary-object
           :+null+))

(defconstant +null+ (code-char 0))

(defun read-null-terminated-ascii (in)
  (with-output-to-string (s)
    (loop for char = (code-char (read-byte in))
          until (char= +null+ char) do (write-char char s))))

(defun write-null-terminated-ascii (string out)
  (loop for char across string
        do (write-byte (char-code char) out))
  (write-byte (char-code +null+) out))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun mklist (x) (if (listp x) x (list x)))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defmacro define-binary-class (name slots)
  `(defclass ,name ()
     ,(mapcar #'slot->defclass-slot slots)))

(defmacro define-binary-class (name slots)
  (with-gensyms (typevar objectvar streamvar)
                `(progn
                   (defclass ,name () 
                     ,(mapcar #'slot->defclass-slot slots))
                   (defmethod read-object progn ((,objectvar ,name) ,streamvar)
                     (with-slots ,(mapcar #'first slots) ,objectvar
                       ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots)))
                   (defmethod write-object progn ((,objectvar ,name) ,streamvar)
                     (with-slots ,(mapcar #'first slots) ,objectvar
                       ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

(defgeneric read-value (type stream &key)
            (:documentation "Read a value from given type from the stream."))

(defgeneric write-value (type stream value &key)
            (:documentation "Write a value as given type to the stream."))

(defgeneric read-object (object stream)
            (:method-combination progn :most-specific-last)
            (:documentation "File in the slots of object form stream."))

(defgeneric write-object (object stream)
            (:method-combination progn :most-specific-last)
            (:documentation "Write out the slots of object to the stream."))

(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))

(defmethod write-value ((type symbol) stream value &key)
  (assert (typep value type))
  (write-object value stream))


