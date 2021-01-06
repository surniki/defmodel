;;;; package.lisp

(defpackage #:defmodel
  (:use #:cl)
  (:export :id
	   :output-stream
	   :current-time
	   :depends-on
	   :time-step
	   :update-model-object
	   :output-model-object-state
	   :open-model-object
	   :close-model-object
	   :defmodel
	   :defprofile
	   :time-step
	   :linked-model-objects
	   :current-time
	   :node
	   :node-name
	   :node-value
	   :node-p
	   :edge
	   :edge-tail
	   :edge-head
	   :edge-value
	   :edge-p
	   :nodes-p
	   :edges-p
	   :digraph
	   :digraph-nodes
	   :digraph-edges
	   :node-name-bound-in-digraph-p
	   :connect-nodes
	   :disconnect-nodes
	   :find-nodes-in-digraph
	   :find-edges-in-digraph
	   :save-digraph
	   :load-digraph
	   :open-model-digraph
	   :close-model-digraph
	   :output-model-digraph-state
	   :update-model-digraph
	   :node-value-from-model-digraph))
