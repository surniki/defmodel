
(in-package #:defmodel)

(defun node (&key name (value nil))
  (make-array 2 :initial-contents (list name value)))

(defun node-name (node)
  (elt node 0))

(defun node-value (node)
  (elt node 1))

(defun node-p (node)
  (and (arrayp node) (= (length node) 2) (symbolp (node-name node))))

(defun edge (&key tail head (value nil))
  (make-array 3 :initial-contents (list tail head value)))

(defun edge-tail (edge)
  (elt edge 0))

(defun edge-head (edge)
  (elt edge 1))

(defun edge-value (edge)
  (elt edge 2))

(defun edge-p (edge)
  (and (arrayp edge) (= (length edge) 3) (symbolp (edge-head edge)) (symbolp (edge-tail edge))))

(defun nodes-p (nodes)
  "T if the given NODES are of the format defined by DIGRAPH, NIL if the NODES are some other kind of object."
  (and (listp nodes) (every #'node-p nodes)))

(defun edges-p (edges)
  "T if the given EDGES are of the format defined by DIGRAPH, NIL if the EDGES are some other kind of object."
  (and (listp edges) (every #'edge-p edges)))

(defun digraph (&key (nodes nil) (edges nil))
  "Constructs an empty directed graph. 
Here, we treat a digraph as a collection of two lists. The first is a list of nodes which are arrays of length 2 that contain
the name of the node and the value of the node. The second is a list of edges, which are arrays of length 3 that contain
the name of the tail node, the name of the head node, and the data that is to be associated with that relationship."
  (if (and (nodes-p nodes) (edges-p edges))
      (make-array 2 :initial-contents (list nodes edges))
      (error "DIGRAPH: The given description is not correctly formatted.")))

(defun digraph-nodes (digraph) (elt digraph 0))
(defun digraph-edges (digraph) (elt digraph 1))
(defun node-name-bound-in-digraph-p (digraph name)
  (some (lambda (node) (eq (node-name node) name)) (digraph-nodes digraph)))

(defun add-edge (digraph &key head tail (value nil))
  (let* ((new-edge (edge :head head :tail tail :value value))
	 (new-edges (append (digraph-edges digraph) (list new-edge))))
    (setf (elt digraph 1) new-edges)
    digraph))

(defun connect-nodes (digraph &key head tail (value nil))
  "Creates an edge in the given DIGRAPH using the ORDERED-PAIR to define the direction of the relationship.
Optionally, the user can give a DATUM argument to associate with the relationship."
  (if (and (node-name-bound-in-digraph-p digraph head)
	   (node-name-bound-in-digraph-p digraph tail))
      (add-edge digraph :head head :tail tail :value value)))

(defun disconnect-nodes (digraph &key head tail (symmetrically-p nil))
  (setf (elt digraph 1) (remove-if (lambda (edge)
				     (or (and (eq (edge-head edge) head)
						  (eq (edge-tail edge) tail))
					 (and symmetrically-p
					      (eq (edge-tail edge) head)
					      (eq (edge-head edge) tail))))
				   (digraph-edges digraph)))
  digraph)

(defun find-nodes-in-digraph (digraph &key name)
  (find-if (lambda (node) (eq (node-name node) name)) (digraph-nodes digraph)))

(defun find-edges-in-digraph (digraph &key name (style :head-to-tail))
  (labels ((find-edges-in-digraph-aux (edges name style result)
	     (if (null edges)
		 result
		 (let ((edge (car edges)))
		   (cond ((eq style :head-to-tail)
			  (find-edges-in-digraph-aux (cdr edges)
						     name
						     style
						     (if (eq (edge-head edge) name) (cons edge result) result)))
			 ((eq style :tail-to-head)
			  (find-edges-in-digraph-aux (cdr edges)
						     name
						     style
						     (if (eq (edge-tail edge) name) (cons edge result) result)))
			 ((eq style :self-loop)
			  (find-edges-in-digraph-aux (cdr edges)
						     name
						     style
						     (if (and (eq (edge-head edge) name)
							      (eq (edge-tail edge) name))
							 (cons edge result)
							 result))))))))
    (find-edges-in-digraph-aux (digraph-edges digraph) name style (list))))
		 
(defun save-digraph (digraph &key filename)
  (with-open-file (output filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (print digraph output)))

(defun load-digraph (filename)
  (with-open-file (input filename :direction :input)
    (read input)))

(defun open-model-digraph (digraph)
  (loop :for node :in (digraph-nodes digraph) :do
       (open-model-object (node-value node))))

(defun close-model-digraph (digraph)
  (loop :for node :in (digraph-nodes digraph) :do
       (close-model-object (node-value node))))

(defun output-model-digraph-state (digraph)
  (loop :for node :in (digraph-nodes digraph) :do
       (output-model-object-state (node-value node))))

(defun update-model-digraph (digraph)
  (loop :for node :in (digraph-nodes digraph) :do
       (let* ((current-obj (node-value node))
	      (edges (find-edges-in-digraph digraph :name (node-name node) :style :head-to-tail))
	      (edge-values (mapcar #'edge-value edges))
	      (tail-objs (mapcar #'node-value
				 (mapcar (lambda (name) (find-nodes-in-digraph digraph :name name))
					 (mapcar #'edge-tail edges))))
	      (linked-objs (mapcar (lambda (x y) (list x y)) edge-values tail-objs)))
	 (update-model-object current-obj linked-objs))))

(defun node-value-from-model-digraph (digraph node-name &optional slot-name)
  (let* ((node (find-nodes-in-digraph digraph :name node-name))
	 (value (node-value node)))
    (if slot-name
	(slot-value value slot-name)
	value)))
