;;;; defmodel.lisp

(in-package #:defmodel)

;; default time step for neurons is 0.01 milliseconds
;; if the user wants this to be changed, they can simply change the time-step value
;; of the class before creating the models and networks.
(defclass model-object ()
   ((output-stream :initform nil :initarg :output-stream)
    (current-time :initform 0 :initarg :current-time)
    (depends-on :initform nil :initarg :depends-on)
    (id :initform nil :initarg :id)
    (time-step :initform 0.01 :allocation :class :initarg :time-step)))

(defmethod print-object ((obj model-object) stream)
  (let* ((class-name (type-of obj))
	 (slot-names (remove-if-not (lambda (name) (slot-boundp obj name))
				    (remove 'output-stream
					    (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (find-class class-name))))))
	 (slot-values (mapcar (lambda (slot-name) (slot-value obj slot-name)) slot-names))
	 (name-value-pairs (mapcar #'list slot-names slot-values)))
    (format stream "#[~a ~:{~a ~a ~}]" class-name name-value-pairs)))

(defgeneric update-model-object (model-object &optional linked-model-objects))
(defgeneric output-model-object-state (model-object))
(defgeneric open-model-object (model-object &key filename))
(defgeneric close-model-object (model-object))

;; defmodel algorithm
;;
;; 1. Take the [var-list] and generate the expected derivative symbols to be defined.
;; 
;; 2. Create two new lists from the [definitions] list; the first is an associative list for the derivative
;;    definitions or variable definitions, and the second is an associative list for the other unnamed definitions
;;    that serve as a shorthand for the user.
;;
;; 3. Check to see that each variable in the [var-list] is defined, and make sure that a variable is not defined by both
;;    a function and a time derivative simultaneously.
;; 
;; 4. Use the shorthand definitions to expand the derivative definitions accordingly. The derivative expressions are now
;;    fully expanded.
;;
;; 5. Generate a class definition for the model.

(define-condition defmodel-too-many-definitions-for-name (error)
  ((variable-name :initarg :name :reader variable-name)
   (model-name :initarg :name :reader model-name))
  (:report (lambda (condition stream)
	     (format stream "Poorly defined variable. TODO: Print symbol and model name."))))

(defmacro defmodel (name var-list &body statements)
  ;; Filter the command statements out for later processing.
  (destructuring-bind (commands definitions) (split #'command-p statements)
    ;; First, we create two new lists from the [definitions] list; the first is an associative list for the derivative
    ;;    definitions or variable definitions, and the second is an associative list for the other unnamed definitions
    ;;    that serve as a shorthand for the user.
    (destructuring-bind (var-defs shorthand-defs) (split (lambda (pair) (variable-definition-p pair var-list)) definitions)
      ;; Next, check to see that each variable in the [var-list] is defined, and make sure that a variable is not defined
      ;; by both a function and a time derivative simultaneously.
      (loop :for symbol :in var-list :do
	   (let ((multiple-defs-variable-error-p (not (only-one-p (lambda (def)
								    (let ((def-sym (first def)))
								      (or (eq def-sym symbol)
									  (eq def-sym (variable->derivative symbol)))))
								  var-defs))))
	     (when multiple-defs-variable-error-p
	       (error 'defmodel-too-many-definitions-for-name :variable-name symbol :model-name name))))
      ;; Then, use the shorthand definitions to expand the derivative definitions accordingly. The derivative expressions
      ;; are now fully expanded.
      (let* ((expanded-var-defs (shorthand-substitution var-defs shorthand-defs))
	     (complete-var-list (append var-list (free-variables var-list expanded-var-defs))))
	;; We also want to create a separation between the [var-defs], depending on whether or not a time derivative
	;; has been described, or if a normal function has been given.
	(destructuring-bind (derivative-var-defs function-var-defs)
	    (split (lambda (pair) (derivative-operands (first pair))) var-defs)
	  ;; Finally, create the class definition for the model.
	  `(progn
	     (defclass ,name (model-object)
	       ,(generate-member-forms complete-var-list))
 
	     ;; If variable is defined using a derivative, then use the entire var-list in its argument list.
	     ;; Otherwise, omit the variable that is being defined from the var-list
	     ,@(mapcar (lambda (variable-def)
			 `(defmethod ,(make-variable-method-name name (first variable-def))
			      ((model-object ,name) current-time ,@var-list &optional linked-model-objects)
			    ,(if (symbol-in-expressionp 'linked-model-objects (second variable-def))
				 '(progn)
				 '(declare (ignore linked-model-objects)))
			    (with-slots ,(free-variables var-list expanded-var-defs) model-object
			      ,(second variable-def))))
		       expanded-var-defs)
	     
	     (defmethod open-model-object ((model-object ,name) &key filename)
	       (with-slots (id output-stream) model-object
		 (let* ((fname (if (null filename)
				   (concatenate 'string (write-to-string id) ".dat")
				   filename)))
		   (setf output-stream (open fname :direction :output :if-does-not-exist :create :if-exists :supersede)))))

	     (defmethod close-model-object ((model-object ,name))
	       (with-slots (output-stream) model-object
		 (close output-stream)))

	     (defmethod update-model-object ((model-object ,name) &optional linked-model-objects)
	       (with-slots ,(append complete-var-list '(current-time time-step)) model-object
		 ,(cond ((and derivative-var-defs function-var-defs)
			 `(progn ,(update-derivative-form name (mapcar #'first derivative-var-defs) var-list)
				 (incf current-time time-step)
				 ,@(update-variable-form name (mapcar #'first function-var-defs) var-list)))
			(derivative-var-defs `(progn ,(update-derivative-form name (mapcar #'first derivative-var-defs) var-list)
						     (incf current-time time-step)))
			(function-var-defs `(progn
					      (incf current-time time-step)
					      ,@(update-variable-form name (mapcar #'first function-var-defs) var-list))))
		 ,(if commands
		      `(progn
			 ,(second (assoc :modify commands))
			 t)
		      t)))
	     
	     (defmethod output-model-object-state ((model-object ,name))
	       (with-slots ,(append var-list '(current-time output-stream)) model-object 
		 (format output-stream ,(n-format-string (+ 1 (length var-list))) current-time ,@var-list)))))))))


  (defmacro defprofile (name model &body definitions)
    (let ((class-name (read-from-string (concatenate 'string (string model) "-" (write-to-string name)))))
      `(defclass ,class-name
	   (,model)
	 ,(generate-static-member-forms definitions))))
