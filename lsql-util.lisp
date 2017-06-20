(defpackage :com.skyline.owl.lsql
  (:use :COMMON-LISP)
  (:nicknames "l-sql" l-sql :l-sql)
  (:export :l-select
           :l-selectr
           :l-delete
           :l-update
           :l-insert))
(in-package :com.skyline.owl.lsql)

(defun l-select (&key (content #'(lambda (a) a)) (from nil) (where #'(lambda (a) t)))
  "This function is to select something from list,like:
  (l-select :CONTENT #'length 
            :FROM '(\"123456\" \"321\" \"444\") 
            :WHERE #'(lambda (x) (> (length x) 3)))"
    (let ((r nil))
        (dolist (obj from)
            (if (funcall where obj)
                (push (funcall content obj) r)))
        (reverse r)))

(defun l-selectr (&key (content #'(lambda (a) a)) (from nil) (where #'(lambda (a) t)))
  "This function is to select something from tree,like:
  (l-selectr :FROM '((1 2 3 4) (5 6 7 8) (11 22 33))
             :WHERE #'(lambda (x) (> x 6)))"
  (let ((r nil))
    (dolist (obj from)
      (if (atom obj)
        (if (funcall where obj)
          (push (funcall content obj) r))
        (push (l-selectr :content content :from obj :where where) r)))
    (reverse r)))

(defun l-insert (item &key (to nil) (before #'(lambda (a) nil)) (after #'(lambda (a) nil)) (at nil))
  "This function is to insert item into list,like:
  (l-insert 333 :TO '(1 2 3 4 5 6 11 22 33) :AT 3)"
  (let ((r nil))
    (do ((i 0 (1+ i)) (obj to (cdr obj)))
      ((or (null obj)
           (and at (>= i at) (push item r) (push (car obj) r)) 
           (and (funcall before (car obj)) (push item r) (push (car obj) r))
           (and (funcall after (car obj)) (push (car obj) r) (push item r))) 
       (and (not (null obj)) (not (null (cdr obj)))  (setf r (append (reverse (cdr obj)) r))))
      (push (car obj) r))
    (reverse r)))

(defun l-update (lst &key (where #'(lambda (a) nil)) (todo #'(lambda (a) a)))
  "This function is to update item in lst  which test :WHERE ok using :TODO,like:
  (l-update '(1 2 3 4 5 6 7 8) :WHERE #'evenp :TODO #'1+)"
  (let ((r nil))
    (dolist (obj lst)
      (if (funcall where obj)
        (push (funcall todo obj) r)
        (push obj r)))
    (reverse r)))

(defun l-delete (lst &key (where #'(lambda (a) nil)))
  "This function is to delete item which test :WHERE ok from lst,like:
  (l-delete '(1 2 3 4 5 6 7) :WHERE #'evenp)"
  (let ((r nil))
    (dolist (obj lst)
      (if (funcall where obj)
        nil
        (push obj r)))
    (reverse r)))

(defun prop-name (&rest props)
  "This function is to provide a function get props from a list,like:
  (prop-name :name :age :id)"
  #'(lambda (lst)
        (let ((r nil))
          (dolist (prop props)
            (push (getf lst prop) r))
          (reverse r))))
