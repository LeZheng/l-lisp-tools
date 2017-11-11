(defpackage :com.skyline.owl.fs
  (:use :COMMON-LISP :EXT)
  (:nicknames :l-fs l-fs "l-fs")
  (:export :list-directory
           :file-exists-p
           :walk-directory
           :file-to-list 
           :list-to-file))

(in-package :com.skyline.owl.fs)

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))
    
(defun directory-pathname-p (p)
  (and 
    (not (component-present-p (pathname-name p)))
    (not (component-present-p (pathname-type p)))
    p))

(defun pathname-as-directory (n)
  (let ((path (pathname n)))
    (when (wild-pathname-p path)
      (error "path error"))
    (if (not (directory-pathname-p n))
      (make-pathname
        :directory (append (or (pathname-directory path) (list :relative))
                           (list (file-namestring path)))
        :name nil
        :type nil
        :defaults path)
      path)))

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathname."))
    (if (directory-pathname-p name)
      (let* ((directory (pathname-directory pathname))
             (name-and-type (pathname (first (last directory)))))
        (make-pathname 
          :directory (butlast directory)
          :name (pathname-name name-and-type)
          :type (pathname-type name-and-type)
          :defaults pathname))
      pathname)))

(defun directory-wildcard (path)
  (make-pathname
    :name :wild
    :type #-clisp :wild #+clisp nil
    :defaults (pathname-as-directory path)))
        
(defun list-directory (dirname)
  "This function is to list dir by dirname,like:
  (list-directory \"/home\")"
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names.*"))
  (let ((wildcard (directory-wildcard dirname)))
    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)
        
    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
      (directory wildcard)
      (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname 
    :directory (append (pathname-directory wildcard) (list :wild))
    :name nil
    :type nil
    :defaults wildcard))

(defun file-exists-p (pathname)
  "This function is to test file exist or not,like:
  (file-exists-p \"/home/admin/a.txt)"
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)
  
  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))
  
  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))
  
  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "list-directory not implemented"))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  "This function is to map fn into items in dirname,like:
  (l-fs:walk-directory \"/home/skyline/temp\" #'(lambda (d) (print d)) :directories t)"
  (labels
    ((walk (name)
           (cond 
             ((directory-pathname-p name)
              (when (and directories (funcall test name))
                (funcall fn name))
              (dolist (x (list-directory name)) (walk x)))
             ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))


(defun file-to-list (filename)
  "This function to read file to list with line,like:
  (file-to-list \"/home/admin/a.txt\")"
  (let ((r nil))
    (with-open-file (str filename :direction :input
                          :if-does-not-exist nil)
        (loop for line = (read-line str nil) while line do (push line r)))
    (reverse r)))
    
(defun list-to-file (lst filename)
  "This function is to write list to file with line,like:
    (list-to-file '(a b c) \"/home/admin/a.txt\")"
  (with-open-file (str filename :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (dolist (line lst)
      (format str "~a" line)
      (write-line "" str))))

(defun ls (path)
  "This function is to list directory in string,like:
  (ls \"/home/admin\")"
  (mapcar #'(lambda (p)  
                (namestring p))  
          (list-directory path)))
