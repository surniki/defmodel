
(ql:quickload :defmodel)

(defpackage defmodel-example
  (:use #:common-lisp #:defmodel))

(in-package #:defmodel-example)

(defmodel lorenz-system (x y z)
  (dx-wrt-dt (* sigma (- y x)))
  (dy-wrt-dt (- (* x (- rho z)) y))
  (dz-wrt-dt (- (* x y) (* beta z))))

(defprofile chaotic lorenz-system
  (rho 28.0)
  (sigma 10.0)
  (beta 8/3))

(defun output-lorenz-attractor ()
  "Simulates the Lorenz system in a chaotic regime."
  (let ((obj (make-instance 'lorenz-system-chaotic ;; make the simulation object
			    :x 2.0
			    :y 2.0
			    :z 2.0
			    :id 'lorenz)))
    (loop :repeat 10000 :do (update-model-object obj)) ;; simulate past the transient
    (open-model-object obj) ;; open the data file labeled via the id slot
    (loop :repeat 5000 :do ;; simulate for time-step x 5000
      (output-model-object-state obj)  ;; print the current state to the data file
      (update-model-object obj)) ;; update the object one time-step
    (close-model-object obj))) ;; finally, close the data file

(output-lorenz-attractor)
