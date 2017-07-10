(defmacro nil! (var)
  `(setq ,var nil))

(defmacro while (test &body body)
  `(do ()
     ((not test))
     ,@body))
