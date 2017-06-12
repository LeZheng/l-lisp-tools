(proclaim '(inline single? append1 map-int filter most best mostn last1 to-be-list longer group flatten prune 
           find2 before after duplicate split-if 
           readlist prompt break-loop 
           mkstr symb reread explode 
           mapa-b map0-n map1-n map-> mappend mapcars rmapcar
           !! def! memoize compose fif fint fun lrec ttrav trec))

(defpackage :com.skyline.owl.tools 
  (:use :COMMON-LISP)
  (:nicknames "lz-tools")
  (:export :single? :append1 :map-int :filter :most :best :mostn :last1 :to-be-list :longer :group :flatten :prune 
           :find2 :before :after :duplicate :split-if 
           :readlist :prompt :break-loop 
           :mkstr :symb :reread :explode 
           :mapa-b :map0-n :map1-n :map-> :mappend :mapcars :rmapcar
           :!! :def! :memoize :compose :fif :fint :fun :lrec :ttrav :trec))

(in-package :com.skyline.owl.tools)
;;;-------------list method-------------
(defun single? (lst)
  (if (consp lst)
    (null (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun map-int (fn num)
  (let ((result nil))
    (dotimes (n num)
      (push (funcall fn n) result))
    (nreverse result)))

(defun filter (fn lst)
  (let ((result nil))
    (dolist (x lst)
      (let ((r (funcall fn x)))
        (and (not (null r)) (push r result) )))
    (nreverse result)))

(defun most (fn lst)
  (let ((result nil)(robj nil))
    (dolist (x lst)
      (let ((s (funcall fn x)))
        (if (null result) 
          (and (setf result s) (setf robj x))
          (and (> s result) (setf result s) (setf robj x)))))
    (values robj result)))

(defun best (fn lst)
  (if (null lst)
    nil
    (let ((win (car lst)))
      (dolist (x (cdr lst))
        (if (funcall fn x win)
          (setf win x)))
      win)))

(defun mostn (fn lst)
  (if (null lst)
    (values nil nil)
    (let ((result (list (car lst))) 
          (m-score (funcall fn (car lst))))
      (dolist (item (cdr lst))
        (let ((score (funcall fn item)))
          (cond 
            ((> score m-score) (setf m-score score) (setf result (list item)))
            ((= score m-score) (push item result))))
        )
      (values (nreverse result) m-score))))


(defun last1 (lst)
  (car (last lst)))

(defun to-be-list (obj)
  (if (listp obj) obj (list obj)))
    
(defun longer (x y)
  (and x
       (if (null y)
         t
         (longer (cdr x) (cdr y)))))
            
(defun group (source n &optional (r nil))
  (let ((result nil) (res nil))
    (do ((i 0 (1+ i))
         (item source (cdr item)))
      ((or (= i n) (null item)) (setf res item))
      (push (car item) result))
    (push (nreverse result) r)
    (if (null res)
      (nreverse r)
      (group res n r))))
            
(defun flatten (x &optional (acc nil))
  (cond 
    ((null x) acc)
    ((atom x) (cons x acc))
    (t (flatten (car x) (flatten (cdr x) acc)))))
        
(defun prune (fn tree &optional (acc nil))
  (cond 
    ((null tree) (nreverse acc))
    ((consp (car tree)) 
     (prune fn (cdr tree) (cons (prune fn (car tree) nil) acc)))
    (t  (prune fn (cdr tree) 
               (if (funcall fn (car tree))
                 acc
                 (cons (car tree) acc))))))

;;;-------------- search -------------------
(defun find2 (fn lst)
  (if (null lst)
    nil
    (let ((val (funcall fn (car lst))))
      (if val
        (values (car lst) val)
        (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((item (car lst)))
         (cond ((funcall test y item) nil)
               ((funcall test x item) lst)
               (before x y (cdr lst) :test test)))))

(defun after (x y lst &key (test #'eql))
  (let ((res (before x y lst :test test)))
    (and res (member y res :test test) )))

(defun duplicate (x lst &key (test #'eql))
  (member x (cdr (member x lst :test test)) :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
      ((or (null src) (funcall fn (car src))) 
       (values (nreverse acc) src))
      (push (car src) acc))))

;;; ------ IO -------
(defun readlist (&rest args) 
  (values (read-from-string (concatenate 'string "(" (apply #'read-line args) ")"))))
    
(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))
    
(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop '~%")
  (loop
    (let ((in (apply #'prompt args))) 
      (if (funcall quit in)
        (return)
        (format *query-io* "~A~%" (funcall fn in))))))

;;;------ STRING ------
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
        
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
    
(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))
    
(defun explode (sym)
  (map 'list #'(lambda (c) (intern (make-string 1 :initial-element c))
                 (symbol-name sym))))

;;;------ MAP -------
(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ a step)) (result nil))
    ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map0-n (fn n)
  (mapa-b fn 0 n))
    
(defun map1-n (fn n)
  (mapa-b fn 1 n))
    
(defun map-> (fn start test-fn succ-fn)
  (do ((i start (+ start step)) (result nil))
    ((funcall test-fn) (nreverse result))
    (push (funcall fn i) result)))
        
(defun mappend (fn &rest lst)
  (apply #'append (apply #'mapcar fn lst)))
    
(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))
        
(defun rmapcar (fn &rest args)
  (if (some #'atom args)
    (apply fn args)
    (apply #'mapcar #'(lambda (&rest args) (apply #'rmapcar fn args)) 
           args)))
           
;;;------ fun constructor ------

(defvar *!equivs* (make-hash-table))

(defun !! (fn)
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
        #'(lambda (&rest args)
            (multiple-value-bind (val win) (gethash args cache)
              (if win
                val
                (setf (gethash args cache) (apply fn args)))))))

(defun compose (&rest fns)
  (if fns
    (let ((fun1 (car (last fns)))
          (funs (butlast fns)))
      #'(lambda (&rest args)
          (reduce #'funcall funs
                  :from-end t
                  :initial-value (apply fun1 args))))
    #'identity))

(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
        (funcall then x)
        (if else (funcall else x)))))

;;function intersection (and)
(defun fint (fn &rest fns)
  (if (null fns)
    fn
    (let ((chain (apply #'fint fns)))
      #'(lambda (x) 
          (and (funcall fn x) (funcall chain x))))))

;;function unintersrction (or)
(defun fun (fn &rest fns)
  (if (null fns)
    fn
    (let ((chain (apply #'fint fns)))
      #'(lambda (x) 
          (or (funcall fn x) (funcall chain x))))))

;;list recurse
(defun lrec (rec &optional base)
  (labels ((self (lst)
                 (if (null lst)
                   (if (functionp base)
                     (funcall base)
                     base)
                   (funcall rec (cdr lst)
                            #'(lambda ()
                                (self (cdr lst)))))))
    #'self))

;;;tree traverser
(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
                 (if (atom tree)
                   (if (functionp base)
                     (funcall base tree)
                     base)
                   (funcall rec (self (car tree))
                            (if (cdr tree)
                              (self (cdr tree)))))))
    #'self))

(defun trec (rec &optional (base #'identity))
  (labels ((self (tree)
                 (if (atom tree)
                   (if (functionp base)
                     (funcall base tree)
                     base)
                   (funcall rec tree
                            #'(lambda () (self (car tree)))
                            #'(lambda () (if (cdr tree) (self (cdr tree))))))))
    #'self))
