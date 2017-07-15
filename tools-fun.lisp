(ql:quickload "cl-ppcre")
(proclaim '(inline single? append1 map-int filter most best mostn last1 to-be-list longer group flatten prune 
           find2 before after duplicate split-if 
           readlist prompt break-loop 
           mkstr symb reread explode 
           mapa-b map0-n map1-n map-> mappend mapcars rmapcar
           !! def! memoize compose fif fint fun lrec ttrav trec))

(defpackage :com.skyline.owl.tools 
  (:use :COMMON-LISP)
  (:nicknames "l-util" l-util :l-util)
  (:export :single? :append1 :map-int :filter :most :best :mostn :last1 :to-be-list :longer :group :classfy :flatten :prune 
           :find2 :before :after :duplicate :split-if 
           :readlist :prompt :break-loop 
           :mkstr :symb :reread :explode 
           :mapa-b :map0-n :map1-n :map-> :mappend :mapcars :rmapcar
           :!! :def! :memoize :compose :fif :fint :fun :lrec :ttrav :trec
           :extract-words))

(in-package :com.skyline.owl.tools)
;;;-------------list method-------------
(defun single? (lst)
  "This function is to test lst whether contain one item,like:
  (single? '(1))"
  (if (consp lst)
    (null (cdr lst))))

(defun append1 (lst obj)
  "This function is to append a item to list,like:
  (append1 '(1 2 3) 4)"
  (append lst (list obj)))

(defun map-int (fn num)
  "This function is to map 0 to num-1 by funcall fn,like:
  (map-int #1+ 10)"
  (let ((result nil))
    (dotimes (n num)
      (push (funcall fn n) result))
    (nreverse result)))

(defun filter (fn lst)
  "This function is to filter item by funcall fn,like:
  (filter #'(lambda (x) (if (evenp x) x)) '(1 2 3 4 5 6 7 8 9))"
  (let ((result nil))
    (dolist (x lst)
      (let ((r (funcall fn x)))
        (and (not (null r)) (push r result) )))
    (nreverse result)))

(defun most (fn lst)
  "This function is to get the item and score which score is highest with funcall fn,like:
  (most #'(lambda (x) (if (> x 0) x (- x))) '(1 -2 3 -4))"
  (let ((result nil)(robj nil))
    (dolist (x lst)
      (let ((s (funcall fn x)))
        (if (null result) 
          (and (setf result s) (setf robj x))
          (and (> s result) (setf result s) (setf robj x)))))
    (values robj result)))

(defun best (fn lst)
  "This function is to get the winner which compare with each other by fn,like:
  (best #'> '(1 -2 3 -4))"
  (if (null lst)
    nil
    (let ((win (car lst)))
      (dolist (x (cdr lst))
        (if (funcall fn x win)
          (setf win x)))
      win)))

(defun mostn (fn lst)
  "This function is to get the items and score which score is highest with funcall fn,like:
  (mostn #'(lambda (x) (if (> x 0) x (- x))) '(1 -2 3 -4 4))"
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
  "This funcion is to get the last car from lst,like:
  (last1 '(1 2 3))"
  (car (last lst)))

(defun to-be-list (obj)
  "This function is to make obj to list,like:
  (to-be-list '(1 2 3)) or (to-be-list 1)"
  (if (listp obj) obj (list obj)))
    
(defun longer (x y)
  "This function is to judge which list is longer than another,like:
  (longer '(1 2 3) '(1 2 3 4))"
  (and x
       (if (null y)
         t
         (longer (cdr x) (cdr y)))))
            
(defun group (source n &optional (r nil))
  "This function is to split list by num,like:
  (group '(1 2 3 4 5 6 7) 3)"
  (let ((result nil) (res nil))
    (do ((i 0 (1+ i))
         (item source (cdr item)))
      ((or (= i n) (null item)) (setf res item))
      (push (car item) result))
    (push (nreverse result) r)
    (if (null res)
      (nreverse r)
      (group res n r))))

(defun associate (obj1 obj2 &key (test #'eql))
  "This function is to associate two object and return a function,like:
  (funcall (associate 1 2) 2 4)"
  #'(lambda (&optional (f #'list)) 
      (funcall f obj1 obj2)))


(defun classfy (items &key (key #'identity) (test #'equal))
  "This function is to classfy items by test key,like:
  (classfy '(1 2 3 4 5 6 7 8) :key #'evenp)"
  (let ((result-table (make-hash-table :test test))
        (result nil))
    (dolist (item items)
      (let ((k (funcall key item)))
        (if (null (gethash k result-table))
          (setf (gethash k result-table) (list item))
          (setf (gethash k result-table) (append (gethash k result-table) (list item))))))
    (maphash #'(lambda (k v) (push v result)) result-table)
    result))
            
(defun flatten (x &optional (acc nil))
  "This function is to extract list in list,like:
  (flatten '(1 2 (3 4 5)))"
  (cond 
    ((null x) acc)
    ((atom x) (cons x acc))
    (t (flatten (car x) (flatten (cdr x) acc)))))
        
(defun prune (fn tree &optional (acc nil))
  "This function is filter to tree,like:
  (prune #'evenp '(1 2 (3 4 5)))"
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
  "This function is to find the item and values which fit in fn,like:
  (find2 #'(lambda (x) (> x 5)) '(4 5 6 7 8))"
  (if (null lst)
    nil
    (let ((val (funcall fn (car lst))))
      (if val
        (values (car lst) val)
        (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  "This function is to judge whether x is before y in lst,like:
  (before 'b 'd '(a b c d))"
  (and lst
       (let ((item (car lst)))
         (cond ((funcall test y item) nil)
               ((funcall test x item) lst)
               (before x y (cdr lst) :test test)))))

(defun after (x y lst &key (test #'eql))
  "This function is to judge whether x is after y in lst,like:
  (after 'd 'b '(a b c d))"
  (let ((res (before x y lst :test test)))
    (and res (member y res :test test) )))

(defun duplicate (x lst &key (test #'eql))
  "This function is to judge the item whether is duplicate in lst,like:
  (duplicate 'a '(a b c d a) :test #'eql)"
  (member x (cdr (member x lst :test test)) :test test))

(defun split-if (fn lst)
  "This function is to split lst by fn,like:
  (split-if #'(lambda (x) (> x 5)) '( 3 4 5 6 7 ))"
  (let ((acc nil))
    (do ((src lst (cdr src)))
      ((or (null src) (funcall fn (car src))) 
       (values (nreverse acc) src))
      (push (car src) acc))))

;;; ------ IO -------
(defun readlist (&rest args) 
  "This function is to readlist from (apply #'read-line args) and concatenate (  ),like:
  (readlist t)"
  (values (read-from-string (concatenate 'string "(" (apply #'read-line args) ")"))))
    
(defun prompt (&rest args)
  "This function is to print msg and read item from *query-io*,like:
  (prompt \"please input ~A number:\" 2)"
  (apply #'format *query-io* args)
  (read *query-io*))
    
(defun break-loop (fn quit &rest args)
  "This function is to provide a env to read and exec,like:
  (break-loop #'eval #'(lambda (x) (eq x :q)) \">> \")"
  (format *query-io* "Entering break-loop '~%")
  (loop
    (let ((in (apply #'prompt args))) 
      (if (funcall quit in)
        (return)
        (format *query-io* "~A~%" (funcall fn in))))))

;;;------ STRING ------
(defun mkstr (&rest args)
  "This function is to mk str by args,like:
  (mkstr 'a '2 3)"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
        
(defun symb (&rest args)
  "This function is to provide a symbol from args,like:
  (symb 'a 'b \"C\")"
  (values (intern (apply #'mkstr args))))
    
(defun reread (&rest args)
  "This function is to provide a lisp obj from args,like:
  (reread 'a 'b 2)"
  (values (read-from-string (apply #'mkstr args))))
    
(defun explode (sym)
  "This function is to provide a list from symbol,like:
  (explode 'abc)"
  (map 'list #'(lambda (c) (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))

;;;------ MAP -------
(defun mapa-b (fn a b &optional (step 1))
  "This function map fn from a to b by step,like:
  (mapa-b #'1+ 5 10)"
  (do ((i a (+ i step)) 
       (result nil))
    ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map0-n (fn n)
  "This function map fn from 0 to n,like:
  (map0-n #'1+ 5)"
  (mapa-b fn 0 n))
    
(defun map1-n (fn n)
  "This function map fn from 1 to n,like:
  (map1-n #1+ 3)"
  (mapa-b fn 1 n))
    
(defun map-> (fn start test-fn succ-fn)
  "This function is to map fn from start to where test-fn step by succ-fn,like:
  (map-> #'evenp 0 #'(lambda (x) (> x 5)) #'1+)"
  (do ((i start (funcall succ-fn i)) (result nil))
    ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))
        
(defun mappend (fn &rest lst)
  "This is no destroy mapcan,like:
   (mappend #'(lambda (x) (and (numberp x) (list x)))
          '(a 1 b c 3 4 d 5))"
  (apply #'append (apply #'mapcar fn lst)))
    
(defun mapcars (fn &rest lsts)
  "This function like mapcar but can input some list,like:
  (mapcars #'sqrt '(1 2 3) '(4 5))"
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))
        
(defun rmapcar (fn &rest args)
  "This function is recursive mapcar,like:
  (rmapcar #'+ '(1 (2 (3) 4)) '(10 (20 (30) 40)))"
  (if (some #'atom args)
    (apply fn args)
    (apply #'mapcar #'(lambda (&rest args) (apply #'rmapcar fn args)) 
           args)))
           
;;;------ fun constructor ------
(defvar *!equivs* (make-hash-table))

(defun !! (fn)
  "This function is to get !fn from *!equivs*"
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  "This function is to def !fn,like:
  (def! #'+ #'-)"
  (setf (gethash fn *!equivs*) fn!))

(defun memoize (fn)
  "This function construct a function having result cache,like:
  (setf m+ (memoize #'+))"
  (let ((cache (make-hash-table :test #'equal)))
        #'(lambda (&rest args)
            (multiple-value-bind (val win) (gethash args cache)
              (if win
                val
                (setf (gethash args cache) (apply fn args)))))))

(defun compose (&rest fns)
  "This function construct a function compose fns by reduce,like:
  (funcall (compose #'sqrt #'1+ #'1+) 2)"
  (if fns
    (let ((fun1 (car (last fns)))
          (funs (butlast fns)))
      #'(lambda (&rest args)
          (reduce #'funcall funs
                  :from-end t
                  :initial-value (apply fun1 args))))
    #'identity))

(defun fif (if then &optional else)
  "This function is to construct a function do if then else,like:
  (funcall (fif #'evenp #'print) 3)"
  #'(lambda (x)
      (if (funcall if x)
        (funcall then x)
        (if else (funcall else x)))))

;;function intersection (and)
(defun fint (fn &rest fns)
  "This function is to provide a function which and fn fns,like:
  (funcall (fint #'numberp #'evenp #'(lambda (x) (> x 5))) 6)"
  (if (null fns)
    fn
    (let ((chain (apply #'fint fns)))
      #'(lambda (x) 
          (and (funcall fn x) (funcall chain x))))))

;;function unintersrction (or)
(defun fun (fn &rest fns)
  "This function construct a function which or fn fns,like:
  (funcall (fun #'evenp #'(lambda (x) (> x 5))) 6)"
  (if (null fns)
    fn
    (let ((chain (apply #'fint fns)))
      #'(lambda (x) 
          (or (funcall fn x) (funcall chain x))))))

;;list recurse
(defun lrec (rec &optional base)
  "This function construct a function recurse list,like:
  (funcall (lrec #'(lambda (x f) (cons x (funcall f)))) '(1 2 3))"
  (labels ((self (lst)
                 (if (null lst)
                   (if (functionp base)
                     (funcall base)
                     base)
                   (funcall rec (car lst)
                            #'(lambda ()
                                (self (cdr lst)))))))
    #'self))

;;;tree traverser
(defun ttrav (rec &optional (base #'identity))
  "This function construct a function traverser tree,like:
  (funcall (ttrav #'nconc #'list) '(1 2 (3 4) 5))"
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
  "This function construct a function traverser tree item,like:
  (funcall (trec #'(lambda (o l r) (nconc (funcall l) (funcall r))) #'list) '(1 2 (3 4) 5))"
  (labels ((self (tree)
                 (if (atom tree)
                   (if (functionp base)
                     (funcall base tree)
                     base)
                   (funcall rec tree
                            #'(lambda () (self (car tree)))
                            #'(lambda () (if (cdr tree) (self (cdr tree))))))))
    #'self))

;;;text about
(defun extract-words (text &optional (size 1))
  "This function is to extract words which longer than size from text,like:
  (extract-words \"hello world\")"
  (delete-duplicates
    (cl-ppcre:all-matches-as-strings 
      (format nil "[a-zA-Z]{~d,}" size) 
      text)
    :test #'string=))
