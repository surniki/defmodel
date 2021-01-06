
(in-package #:defmodel)

(defun alternate-keywords (args)
  (labels ((yes-keyword (args result)
	     (if (null args)
		 (reverse result)
		 (no-keyword (rest args) (cons (make-keyword (first args)) result))))
	   (no-keyword (args result)
	     (if (null args)
		 (error "Odd number of arguments in object literal expression.")
		 (yes-keyword (rest args) (cons (first args) result)))))
    (yes-keyword args (list))))
		 

(defun object-reader (stream char1 char2)
  (declare (ignore char1 char2))
  (let* ((data (read-delimited-list #\] stream t))
	 (class-name (first data))
	 (arguments (rest data)))
    `(make-instance (quote ,class-name) ,@(alternate-keywords arguments))))

(set-macro-character #\] (get-macro-character #\)))

(set-dispatch-macro-character #\# #\[ #'object-reader)
			      
