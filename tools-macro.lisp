(defpackage :com.skyline.owl.lmacro
  (:use "COMMON-LISP")
  (:nicknames "l-macro" :l-macro l-macro)
  (:export :nil!
           :while
           :for
           :bind-when
           :bind-when*))

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

(defmacro bind-when ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro bind-when* (binds &body body)
  (if (null binds)
    `(prog ,@body)
    `(let (,(car binds))
       (if ,(caar binds)
         (bind-when* (cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)
     ,@body))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
                (let ,(condlet-binds vars cl)
                  (,bodfn ,@(mapcar #'cdr vars))))))

(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform) 
              (if (consp bindform)
                (cons (cdr (assoc (car bindform) vars))
                      (cdr bindform))))
          (cdr cl)))

(defmacro condlet (clauses & body)
  (let ((bodfn (gensym))
        (vars (mapcar #'(lambda (v) (cons v (gensym)))
                      (remove-duplicates
                        (mapcar #'car (mappend #'car clauses))))))
    `(labels ((,bodfn ,(mapcar #'(lambda (cl)
                                   (condlet-clause vars cl bodfn))
                               clauses))))))
