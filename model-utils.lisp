
(in-package #:defmodel)

(defun boolean<- (value)
  (not (null value)))

(defparameter *commands* '(:modify))

(defun command-p (pair)
  "Is the pair a defmodel statement that evokes a command?"
  (member (first pair) *commands*))

(defun symbol-in-expressionp (symbol exp)
  (labels ((symbol-in-expressionp-aux (symbol exp)
	     (if (null exp)
	       nil
	       (let ((term (first exp)))
		 (cond ((symbolp term)
			(or (eq term symbol) (symbol-in-expressionp symbol (rest exp))))
		       ((listp term)
			(or (symbol-in-expressionp symbol term)
			    (symbol-in-expressionp symbol (rest exp))))
		       (t (symbol-in-expressionp symbol (rest exp))))))))
    (if (listp exp)
	(symbol-in-expressionp-aux symbol exp)
	(eq symbol exp))))
	       

(defun derivative-operands (symbol)
  "Is the SYMBOL a valid derivative name?
If so, then return a list of the left and right derivative operands. Otherwise, return nil."
  (let* ((symbol-str (string symbol))
	 (length (length symbol-str))
	 (separator (string '-wrt-d))
	 (wrt-location (search separator symbol-str))
	 (first-char-correct (char= (aref symbol-str 0) #\D)))
    (when (and wrt-location first-char-correct)
      (let ((well-formed-wrt (> (- length (+ wrt-location (length separator))) 0)))
	(when well-formed-wrt
	  (let ((left-operand (subseq symbol-str 1 wrt-location))
		(right-operand (subseq symbol-str (+ (length separator) wrt-location) length)))
	    (list (read-from-string (string-trim " -" left-operand))
		  (read-from-string (string-trim " -" right-operand)))))))))

(defun variable->derivative (var-sym)
  "Constructs a derivative symbol from the variable VAR-SYM."
  (read-from-string (concatenate 'string
				 "d"
				 (let ((str (string var-sym)))
				   (if (= (length str) 1)
				       str
				       (concatenate 'string "-" str)))
				 "-wrt-dt")))

(defun variables->derivatives (variables)
  "Plural form of VARIABLE-DERIVATIVES, acting on the VARIABLES list."
  (mapcar #'variable->derivative variables))

(defun free-variables (bound-symbols expr)
  "Returns a list of free variables given a list of bound symbols and an expression to search."
  (labels ((free-variables-aux (expr free-vars)
	     (let ((current-symbol (car expr)))
		 (cond
		   ((null current-symbol)
		    free-vars)
		   ((listp current-symbol)
		    (free-variables-aux (cdr expr)
					(append free-vars
						(free-variables-aux current-symbol nil))))
		   ((symbolp current-symbol)
		    (free-variables-aux (cdr expr)
					(if (or (member current-symbol bound-symbols)
						(member current-symbol
							(variables->derivatives bound-symbols))
						(boundp current-symbol)
						(fboundp current-symbol)
						(member current-symbol free-vars)
						(eq current-symbol 'linked-model-objects)
						(eq current-symbol 'current-time))
					    free-vars
					    (cons current-symbol free-vars))))
		   (t (free-variables-aux (cdr expr) free-vars))))))
      (remove-duplicates (free-variables-aux expr nil))))


(defun symbol-value-pair-p (value)
  "Is the VALUE an association list member?"
  (boolean<-
   (and (= (length value) 2)
	(symbolp (first value)))))

(defun variable-definition-p (symbol-value-pair variables)
  "Is the preposed SYMBOL-VALUE-PAIR a definition for a member of the VARIABLES list?"
  (boolean<-
   (and (symbol-value-pair-p symbol-value-pair)
	(let ((symbol (first symbol-value-pair)))
	  (or (member symbol variables)
	      (member symbol (variables->derivatives variables)))))))

(defun split (predicate list)
  "Returns a list of two lists of elements sorted using the given PREDICATE from the given LIST.
The first list is the values for which the predicate holds. The second list is the values for which the predicate does not hold."
  (list (remove-if-not predicate list)
	(remove-if predicate list)))

(defun only-one-p (predicate list)
  "T if there is only one member of the list for which the PREDICATE holds, and NIL otherwise."
  (= 1 (reduce '+ (mapcar (lambda (x) (if x 1 0)) (mapcar predicate list)))))

(defparameter *shorthand-substitution-recursion-limit* 100)

(defun shorthand-substitution (var-defs shorthand-defs)
  "Constructs a new list of variable definitions using VAR-DEFS as a starting point and SHORTHAND-DEFS as a list of substitution rules. This does a deep substitution; because of this, circular dependencies in the SHORTHAND-DEFS list will
result in infinite recursion."
  (labels ((shorthand-substitution-aux (var-defs shorthand-defs recursion-depth)
	     (if (> recursion-depth *shorthand-substitution-recursion-limit*)
		 (error
		  (format nil "Recursion depth for SHORTHAND-SUBSTITUTION exceeded limit as defined by *SHORTHAND-SUBSTITUTION-RECURSION-LIMIT*. (currently ~A)"
			  *shorthand-substitution-recursion-limit*))
		 (let ((new-var-defs (copy-list var-defs)))
		   (loop :for shorthand-def :in shorthand-defs :do
			(let ((shorthand-name (first shorthand-def))
			      (shorthand-value (second shorthand-def)))
			  (setq new-var-defs (subst shorthand-value shorthand-name new-var-defs))))
		   (if (equal new-var-defs var-defs)
		       new-var-defs
		       (shorthand-substitution-aux new-var-defs shorthand-defs (+ 1 recursion-depth)))))))
    (shorthand-substitution-aux var-defs shorthand-defs 0)))

(defun make-keyword (symbol)
  "Creates a keyword that has the same name as SYMBOL."
  (read-from-string (concatenate 'string ":" (string symbol))))

(defun generate-member-forms (member-symbol-list)
  "Generate a list of member forms for use in DEFCLASS from a list of symbols representing the needed data members."
  (labels ((generate-member-form (member-symbol)
	     (list member-symbol :initarg (make-keyword member-symbol)))
	   (generate-member-forms-aux (member-symbol-list result)
	     (if (null member-symbol-list)
		 result
		 (generate-member-forms-aux (cdr member-symbol-list)
					    (cons (generate-member-form (car member-symbol-list)) result)))))
    (generate-member-forms-aux member-symbol-list nil)))

(defun n-format-string (n)
    (let ((result ""))
      (dotimes (i n)
	(setf result (concatenate 'string result "~A ")))
      (concatenate 'string result "~%")))

(defun update-variable-form (name variable-var-list var-list)
  (let ((variable-fun-list (mapcar (lambda (var) (make-variable-method-name name var)) variable-var-list)))
    (mapcar (lambda (var fun)
	      `(setf ,var (,fun model-object current-time ,@var-list linked-model-objects)))
	    variable-var-list variable-fun-list)))

(defun derivative-application-form (left-op model-name d-vars var-list runge-kutta-vec-name)
  (let ((var-list-with-d (mapcar (lambda (var) (if (member var d-vars) (variable->derivative var) var)) var-list))
	(func-name (read-from-string (concatenate 'string (string model-name) "-" (string (variable->derivative left-op))))))
    (cond ((eq runge-kutta-vec-name 'k1)
	   `(* time-step (,func-name model-object current-time ,@var-list linked-model-objects)))
	  ((eq runge-kutta-vec-name 'k2)
	   `(* time-step (,func-name model-object
				     (+ current-time (/ time-step 2.0))
				     ,@(mapcar (lambda (var)
						 (let ((d-left-op (first (derivative-operands var))))
						   (if d-left-op
						       `(+ ,d-left-op (/ (svref k1 ,(position d-left-op d-vars)) 2.0))
						       `(,(make-variable-method-name model-name var)
							  model-object
							  (+ current-time (/ time-step 2.0))
							  ,@var-list
							  linked-model-objects))))
					       var-list-with-d))))
	   ((eq runge-kutta-vec-name 'k3)
	    `(* time-step (,func-name model-object
				      (+ current-time (/ time-step 2.0))
				      ,@(mapcar (lambda (var)
						  (let ((d-left-op (first (derivative-operands var))))
						    (if d-left-op
							`(+ ,d-left-op (/ (svref k2 ,(position d-left-op d-vars)) 2.0))
							`(,(make-variable-method-name model-name var)
							   model-object
							   (+ current-time (/ time-step 2.0))
							   ,@var-list
							   linked-model-objects))))
						var-list-with-d))))
	    ((eq runge-kutta-vec-name 'k4)
	     `(* time-step (,func-name model-object
			(+ current-time time-step)
			,@(mapcar (lambda (var)
				    (let ((d-left-op (first (derivative-operands var))))
				      (if d-left-op
					  `(+ ,d-left-op (svref k3 ,(position d-left-op d-vars)))
					  `(,(make-variable-method-name model-name var)
					     model-object
					     (+ current-time time-step)
					     ,@var-list
					     linked-model-objects))))
				  var-list-with-d))))
	  (t 'derivative-application-form-error))))

;; TODO: the update functions have to be evaluated with the same number of arguments as the var-list.
(defun update-derivative-form (name derivative-var-list var-list)
 (let ((vars-with-derivative (mapcar (lambda (der-name) (first (derivative-operands der-name)))
				      derivative-var-list)))
    `(let* ((k1 (vector ,@(mapcar (lambda (d-var)
				    (derivative-application-form d-var name vars-with-derivative var-list 'k1))
				  vars-with-derivative)))
	    (k2 (vector ,@(mapcar (lambda (d-var)
				    (derivative-application-form d-var name vars-with-derivative var-list 'k2))
		      vars-with-derivative)))
	    (k3 (vector ,@(mapcar (lambda (d-var)
				    (derivative-application-form d-var name vars-with-derivative var-list 'k3))
		      vars-with-derivative)))
	    (k4 (vector ,@(mapcar (lambda (d-var)
				    (derivative-application-form d-var name vars-with-derivative var-list 'k4))
				  vars-with-derivative))))
       ,@(mapcar (lambda (d-var)
		   (let ((index (position d-var vars-with-derivative)))
		     `(setf ,d-var (+ ,d-var
				      (/ (svref k1 ,index) 6.0)
				      (/ (svref k2 ,index) 3.0)
				      (/ (svref k3 ,index) 3.0)
				      (/ (svref k4 ,index) 6.0)))))
		 vars-with-derivative))))

(defun generate-static-member-forms (symbol-value-pairs)
  (labels ((generate-static-member-form (symbol-value-pair)
	     (let ((symbol (first symbol-value-pair))
		   (value (second symbol-value-pair))) 
	       (list symbol :initform value :allocation :class)))
	   (generate-static-member-forms-aux (symbol-value-pairs result)
	     (if (null symbol-value-pairs)
		 result
		 (generate-static-member-forms-aux (cdr symbol-value-pairs)
						   (cons (generate-static-member-form (car symbol-value-pairs)) result)))))
    (reverse (generate-static-member-forms-aux symbol-value-pairs nil))))

(defun make-variable-method-name (name var)
  (read-from-string (concatenate 'string (string name) "-" (string var))))
