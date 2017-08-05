(defpackage :com.skyline.owl.lmacro
  (:use "COMMON-LISP")
  (:nicknames "l-macro" :l-macro l-macro)
  (:export :nil!
           :while
           :for))

(in-package :com.skyline.owl.lmacro)

(defmacro nil! (var)
  `(setf ,var nil))

(defmacro while (test &body body)
  `(do ()
     ((not test))
     ,@body))

(defmacro for (((var start) stop step) &body body)
  `(do ((,var ,start (funcall ,step ,var)))
     (,stop)
     ,@body))
