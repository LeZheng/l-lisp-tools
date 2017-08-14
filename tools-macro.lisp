(defpackage :com.skyline.owl.lmacro
  (:use "COMMON-LISP")
  (:nicknames "l-macro" :l-macro l-macro)
  (:export :nil!
           :while
           :for
           :bind-when
           :bind-when*
           :if3 :nif :in-if :in :inq :>case
           :till :do-tuples/c :do-tuples/o))

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

(defmacro if3 (test tc nc ?c)
  `(case (test)
     ((nil) ,nc)
     (t ,tc)
     (? ,?c)))

(defmacro nif (expr plus zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,plus)
             ((zerop ,g) ,zero)
             (t ,neg)))))

(defmacro in (obj &rest choises)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) '(eql ,insym ,c)) choises))))

(defmacro inq (obj &rest choises)
  `(in ,obj ,@(mapcar #'(lambda (a) `',a) choises)))

(defmacro in-if (fn &rest choises)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (x) (funcall ,fnsym x)) choises)))))

(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar #'(lambda (x) (>casex g cl)) clauses)))))

(defmacro >casex (g cl)
  (let ((key (car cl)) (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq key t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))

(defmacro till (test &body body)
  `(do ()
     (,test)
     ,@body))

(defmacro do-tuples/o (params source &body body)
  (if params
    (let ((src (gensym)))
      `(prog ((,src ,source))
             (mapc #'(lambda ,params ,@body)
                   ,@(map0-n #'(lambda (n) 
                                 `(nthcdr ,n ,src))
                             (- (length source)
                                (length params))))))))

(defmacro do-tuples/c (params source &body body)
  (if params
    (with-gensyms (src rest bodfn)
        (let ((len (length params)))
          `(let ((,src ,source))
             (when (nthcdr ,(1- len) ,src)
               (labels ((,bodfn ,params ,@body))
                 (do ((,rest ,src (cdr ,rest)))
                   ((not (nthcdr ,(1- len) ,rest))
                    ,@(mapcar #'(lambda (args)
                                  `(,bodfn ,@args))
                              (dt-args len rest src))
                    nil)
                   (,bodfn ,@(map1-n #'(lambda (n) `(nth ,(1- n)
                                                         ,rest))
                                     len))))))))))

(defun dt-args (len rest src)
  (map0-n #'(lambda (m) 
              (map1-n #'(lambda (n)
                          (let ((x (+ m n)))
                            (if (>= x len)
                              `(nth ,(-x len) ,src)
                              `(nth ,(1- x) ,rest))))
                      len))
          (- len 2)))

(defmacro mvdo* (param-cl test-cl &body body)
  (mvdo-gen param-cl param-cl test-cl body))

(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
    (let ((label (gensym)))
      `(prog nil
             ,label
             (if ,(car test)
               (return (progn ,@(cdr test))))
             ,@body
             ,@(mvdo-rebind-gen rebinds)
             (go ,label)))
    (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
      (let ((var/s (caar binds)) (expr (cadar binds)))
        (if (atom var/s)
          `(let ((,var/s ,expr)) ,rec)
          `(multiple-value-bind ,var/s ,expr ,rec))))))

(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
        ((< (length (car rebinds)) 3)
         (mvdo-rebind-gen (cdr rebinds)))
        (t
          (cons (list (if (atom (caar rebinds))
                        'setq
                        'multiple-value-setq)
                      (caar rebinds)
                      (third (car rebinds)))
                (mvdo-rebind-gen (cdr rebinds))))))
