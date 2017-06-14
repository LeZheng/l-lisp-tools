(defpackage :com.skyline.owl.lsql
  (:use :COMMON-LISP)
  (:nicknames "l-sql")
  (:export :l-select
           :l-selectr
           :l-delete
           :l-update
           :l-insert))
(in-package :com.skyline.owl.lsql)

(defun file-to-list (filename)
  (let ((r nil))
    (with-open-file (str filename :direction :input
                          :if-does-not-exist nil)
        (loop for line = (read-line str nil) while line do (push line r)))
    (reverse r)))
    
(defun list-to-file (lst filename)
  (with-open-file (str filename :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (dolist (line lst)
      (format str "~a" line)
      (write-line "" str))))

(defun l-select (&key (content #'(lambda (a) a)) (from nil) (where #'(lambda (a) t)))
    (let ((r nil))
        (dolist (obj from)
            (if (funcall where obj)
                (push (funcall content obj) r)))
        (reverse r)))

(defun l-selectr (&key (content #'(lambda (a) a)) (from nil) (where #'(lambda (a) t)))
  (let ((r nil))
    (dolist (obj from)
      (if (atom obj)
        (if (funcall where obj)
          (push (funcall content obj) r))
        (push (l-selectr :content content :from obj :where where) r)))
    (reverse r)))

(defun l-insert (item &key (to nil) (before #'(lambda (a) nil)) (after #'(lambda (a) nil)) (at nil))
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
  (let ((r nil))
    (dolist (obj lst)
      (if (funcall where obj)
        (push (funcall todo obj) r)
        (push obj r)))
    (reverse r)))

(defun l-delete (lst &key (where #'(lambda (a) nil)))
  (let ((r nil))
    (dolist (obj lst)
      (if (funcall where obj)
        nil
        (push obj r)))
    (reverse r)))

(defun prop-name (&rest props)
  #'(lambda (lst)
        (let ((r nil))
          (dolist (prop props)
            (push (getf lst prop) r))
          (reverse r))))
;;;----------------example---------------------
(l-select :from '(1 2 3 4 5)
        :where #'(lambda (a) (evenp a)))

(l-selectr :from '(1 2 3 4 (6 4 3) 8 (6 (4 44))) 
         :where #'(lambda (a) (evenp a)))
