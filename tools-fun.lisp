(proclaim '(inline single? append1 map-int filter most lst-length last1))

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
					(setf result s)
					(and (> s result) (setf result s) (setf robj x)))))
		(values robj result)))

(defun lst-length (lst)
	(if (consp lst) 
		(+ 1 (lst-length (cdr lst)))
		0))
		
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
