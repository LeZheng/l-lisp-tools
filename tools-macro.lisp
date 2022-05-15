(defpackage :com.skyline.owl.lmacro
  (:use "COMMON-LISP")
  (:nicknames "l-macro" :l-macro l-macro)
  (:export :nil!
           :while
           :for
           :with-gensyms
           :when-bind
           :when-bind*
           :if3 :nif :in-if :in :inq :>case
           :till :do-tuples/c :do-tuples/o))

(in-package :com.skyline.owl.lmacro)

(defmacro nil! (var)
  `(setf ,var nil))

(defmacro while (test &body body)
  `(do ()
     ((not ,test))
     ,@body))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
	 ,&body)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

;;;(when-bind* ((x (find-if #’consp ’(a (1 2) b)))
;;;	      (y (find-if #’oddp x)))
;;;	      (+ y 10))

(defmacro when-bind* (binds &body body)
  (if (null binds)
    `(prog ,@body)
    `(let (,(car binds))
       (if ,(caar binds)
         (bind-when* ,(cdr binds) ,@body)))))

;;;(defmacro with-redraw ((var objs) &body body)
;;;  (with-gensyms (gob x0 y0 x1 y1)
;;;		...))
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

;;;(condlet (((= 1 2) (x (princ ’a)) (y (princ ’b)))
;;;	    ((= 1 1) (y (princ ’c)) (x (princ ’d)))
;;;	    (t
;;;	    (x (princ ’e)) (z (princ ’f))))
;;;	 (list x y z))
(defmacro condlet (clauses &body body)
  (let ((bodfn (gensym))
        (vars (mapcar #'(lambda (v) (cons v (gensym)))
                      (remove-duplicates
                        (mapcar #'car (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
                      ,@body))
       (cond ,@(mapcar #'(lambda (cl)
                                   (condlet-clause vars cl bodfn))
                               clauses)))))

(defmacro if3 (test t-case nil-case ?-case)
  `(case ,test
     ((nil) ,nil-case)
     (? ,?-case)
     (t ,t-case)))

(defmacro nif (expr plus zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,plus)
             ((zerop ,g) ,zero)
             (t ,neg)))))

(defmacro in (obj &rest choises)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c)) choises)))))

(defmacro inq (obj &rest choises)
  `(in ,obj ,@(mapcar #'(lambda (a) `',a) choises)))

(defmacro in-if (fn &rest choises)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (x) `(funcall ,fnsym ,x)) choises)))))

(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar #'(lambda (cl) (>casex g cl)) clauses)))))

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

(defmacro mvpsetq (&rest args)
  (let* ((pairs (group args 2))
         (syms (mapcar #'(lambda (p)
                                 (mapcar #'(lambda (x) (gensym))
                                         (mklist (car p))))
                       pairs)))
    (labels ((rec (ps ss)
                  (if (null ps)
                    `(setq
                         ,@(mapcan #'(lambda (p s)
                                             (shuffle (mklist (car p))
                                                      s))
                                   pairs syms))
                    (let ((body (rec (cdr ps) (cdr ss))))
                      (let ((var/s (caar ps))
                            (expr (cadar ps)))
                        (if (consp var/s)
                          `(multiple-value-bind ,(car ss)
                                                ,expr
                                                ,body)
                          `(let ((,@(car ss) ,expr))
                                ,body)))))))
      (rec pairs syms))))

(defun shuffle (x y)
  (cond ((null x) y)
        ((null y) x)
        (t (list* (car x) (car y) (shuffle (cdr x) (cdr y))))))

(defmacro mvdo (binds (test &rest result) &body body)
  (let ((label (gensym))
        (temps (mapcar #'(lambda (b)
                                 (if (listp (car b))
                                   (mapcar #'(lambda (x)
                                                     (gensym))
                                           (car b))
                                   (gensym)))
                       binds)))
    `(let ,(mappend #'mklist temps)
          (mvpsetq ,@(mapcan #'(lambda (b var)
                                       (list var (cadr b)))
                             binds
                             temps))
          (prog ,(mapcar #'(lambda (b var) (list b var))
                         (mappend #'mklist (mapcar #'car binds))
                         (mappend #'mklist temps))
                ,label
                (if ,test
                  (return (progn ,@result)))
                ,@body
                (mvpsetq ,@(mapcan #'(lambda (b)
                                             (if (third b)
                                               (list (car b)
                                                     (third b))))
                                   binds))
                (go ,label)))))

;;;12.1 
(defmacro allf (val &rest args)
  (with-gensyms (gval)
		‘(let ((,gval ,val))
		    (setf ,@(mapcan #’(lambda (a) (list a gval))
				       args)))))

(defmacro nilf (&rest args) ‘(allf nil ,@args))

(defmacro tf (&rest args) ‘(allf t ,@args))

(defmacro toggle (&rest args)
  ‘(progn
      ,@(mapcar #’(lambda (a) ‘(toggle2 ,a))
		   args)))

(define-modify-macro toggle2 () not)


;;;12.2
(define-modify-macro concf (obj) nconc)

(defun conc1f/function (place obj)
  (nconc place (list obj)))

(define-modify-macro conc1f (obj) conc1f/function)

(defun concnew/function (place obj &rest args)
  (unless (apply #’member obj place args)
    (nconc place (list obj))))

(define-modify-macro concnew (obj &rest args)
  concnew/function)

;;;12.3
(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
		       (get-setf-expansion place)
		       ‘(let* (,@(mapcar #’list vars forms)
				(,(car var) (,op ,access ,@args)))
			   ,set)))

(defmethod pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
		       (get-setf-expansion place)
		       (let ((g (gensym)))
			 ‘(let* ((,g ,obj)
				  ,@(mapcar #’list vars forms)
				  (,(car var) (delete ,g ,access ,@args)))
			     ,set))))

(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access)
		       (get-setf-expansion place)
		       (let ((g (gensym)))
			 ‘(let* ((,g ,test)
				  ,@(mapcar #’list vars forms)
				  (,(car var) (delete-if ,g ,access ,@args)))
			     ,set))))

(defmacro popn (n place)
  (multiple-value-bind (vars forms var set access)
		       (get-setf-expansion place)
		       (with-gensyms (gn glst)
				     ‘(let* ((,gn ,n)
					      ,@(mapcar #’list vars forms)
					      (,glst ,access)
					      (,(car var) (nthcdr ,gn ,glst)))
					 (prog1 (subseq ,glst 0 ,gn)
					   ,set)))))

;;;12.4
(defmacro sortf (op &rest places)
  (let* ((meths (mapcar #’(lambda (p)
			     (multiple-value-list
			      (get-setf-expansion p)))
			   places))
	 (temps (apply #’append (mapcar #’third meths))))
    ‘(let* ,(mapcar #’list
			(mapcan #’(lambda (m)
				     (append (first m)
					     (third m)))
				   meths)
			(mapcan #’(lambda (m)
				     (append (second m)
					     (list (fifth m))))
				   meths))
	,@(mapcon #’(lambda (rest)
		       (mapcar
			#’(lambda (arg)
			     ‘(unless (,op ,(car rest) ,arg)
				 (rotated ,(car rest) ,arg)))
			(cdr rest)))
		     temps)
	,@(mapcar #’fourth meths))))

;;;14.1
(defmacro aif (test-form then-form &optional else-form)
  ‘(let ((it ,test-form))
      (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  ‘(aif ,test-form
	 (progn ,@body)))

(defmacro awhile (expr &body body)
  ‘(do ((it ,expr ,expr))
	((not it))
	,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t ‘(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (sym (gensym)))
      ‘(let ((,sym ,(car cl1)))
	  (if ,sym
	      (let ((it ,sym)) ,@(cdr cl1))
	    (acond ,@(cdr clauses)))))))

;;;14.2
(defmacro alambda (parms &body body)
  ‘(labels ((self ,parms ,@body))
	    #’self))

(defmacro ablock (tag &rest args)
  ‘(block ,tag
	   ,(funcall (alambda (args)
			      (case (length args)
				    (0 nil)
				    (1 (car args))
				    (t ‘(let ((it ,(car args)))
					   ,(self (cdr args))))))
		     args)))

;;;14.3
(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    ‘(multiple-value-bind (it ,win) ,test
			   (if (or it ,win) ,then ,else))))

(defmacro awhen2 (test &body body)
  ‘(aif2 ,test
	  (progn ,@body)))

(defmacro awhile2 (test &body body)
  (let ((flag (gensym)))
    ‘(let ((,flag t))
	(while ,flag
	  (aif2 ,test
		(progn ,@body)
		(setq ,flag nil))))))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (val (gensym))
	  (win (gensym)))
      ‘(multiple-value-bind (,val ,win) ,(car cl1)
			     (if (or ,val ,win)
				 (let ((it ,val)) ,@(cdr cl1))
			       (acond2 ,@(cdr clauses)))))))

;;;14.4
(let ((g (gensym)))
  (defun read2 (&optional (str *standard-input*))
    (let ((val (read str nil g)))
      (unless (equal val g) (values val t)))))

(defmacro do-file (filename &body body)
  (let ((str (gensym)))
    ‘(with-open-file (,str ,filename)
		      (awhile2 (read2 ,str)
			       ,@body))))

;;;15.1
(defmacro fn (expr) ‘#’,(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) ’lambda))
      expr
    (if (eq (car expr) ’compose)
	(build-compose (cdr expr))
      (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    ‘(lambda (,g)
	(,op ,@(mapcar #’(lambda (f)
			    ‘(,(rbuild f) ,g))
			  fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    ‘(lambda (,g)
	,(labels ((rec (fns)
		       (if fns
			   ‘(,(rbuild (car fns))
			      ,(rec (cdr fns)))
			 g)))
		 (rec fns)))))

;;;15.2
(defmacro alrec (rec &optional base)
  ”cltl2 version”
  (let ((gfn (gensym)))
    ‘(lrec #’(lambda (it ,gfn)
		 (symbol-macrolet ((rec (funcall ,gfn)))
				  ,rec))
	       ,base)))

(defmacro on-cdrs (rec base &rest lsts)
  ‘(funcall (alrec ,rec #’(lambda () ,base)) ,@lsts))

;;;15.4
(defun unions (&rest sets)
  (on-cdrs (union it rec) (car sets) (cdr sets)))

(defun intersections (&rest sets)
  (unless (some #’null sets)
    (on-cdrs (intersection it rec) (car sets) (cdr sets))))

(defun differences (set &rest outs)
  (on-cdrs (set-difference rec it) set outs))

(defun maxmin (args)
  (when args
    (on-cdrs (multiple-value-bind (mx mn) rec
				  (values (max mx it) (min mn it)))
	     (values (car args) (car args))
	     (cdr args))))

;;;15.5
(defmacro atrec (rec &optional (base ’it))
  ”cltl2 version”
  (let ((lfn (gensym)) (rfn (gensym)))
    ‘(trec #’(lambda (it ,lfn ,rfn)
		 (symbol-macrolet ((left (funcall ,lfn))
				   (right (funcall ,rfn)))
				  ,rec))
	       #’(lambda (it) ,base))))

(defmacro on-trees (rec base &rest trees)
  ‘(funcall (atrec ,rec ,base) ,@trees))

;;;15.7
(defconstant unforced (gensym))

(defstruct delay forced closure)

(defmacro delay (expr)
  (let ((self (gensym)))
    ‘(let ((,self (make-delay :forced unforced)))
	(setf (delay-closure ,self)
	      #’(lambda ()
		   (setf (delay-forced ,self) ,expr)))
	,self)))

(defun force (x)
  (if (delay-p x)
      (if (eq (delay-forced x) unforced)
	  (funcall (delay-closure x))
	(delay-forced x))
    x))
