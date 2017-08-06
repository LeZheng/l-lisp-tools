(defpackage :com.skyline.owl.text
  (:nicknames l-text :l-text "l-text")
  (:use :common-lisp :com.skyline.owl.tools)
  (:export :extract-words :extract-features :parse-text))

(in-package :com.skyline.owl.text)

(defvar *feature-database* (make-hash-table :test #'equal))

(defclass word-feature ()
  ((word
     :initarg :word
     :accessor word
     :initform (error "Must supply :word")
     :documentation "The word this feature represents.")
   (type-list
     :initarg :types
     :accessor types
     :initform nil
     :documentation "The slots means this word have appeared in these type.")))

(defclass type-feature ()
  ((type
     :initarg :type
     :accessor type
     :initform (error "Must supply :sentence"))
   (key-list
     :initarg :keys
     :accessor keys
     :initform nil)))

(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word types) object
      (format stream "~s :type-list ~{~a  ~}" word types))))

(defun parse-text (text)
  "This function is parse text and return results as list"
  (classfy (reduce #'append (mapcar #'feature-score (extract-features text)))
    :key #'(lambda (x) (car x)))
  ;;TODO text => '((type1 score1) (type2 score2))
  )

(defun feature-score (feature)
  (list (list (word feature) 1))
  ;;TODO feature => '((type1 score1) (type2 score2))
  )

(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))

;;;text about
(defun extract-words (text &optional (size 1))
  "This function is to extract words which longer than size from text,like:
  (extract-words \"hello world\")"
  (delete-duplicates
    (cl-ppcre:all-matches-as-strings
      (format nil "[a-zA-Z]{~d,}" size)
      text)
    :test #'string=))

(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text 3)))

(defun clear-database ()
  (setf *feature-database* (make-hash-table :test #'equal)))
