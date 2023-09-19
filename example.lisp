
(in-package :cl-user)

;;(load "cl-const-generics.asd")
;;(ql:quickload :cl-const-generics)

(defpackage :example
  (:use :cl :cl-const-generics))

(in-package :example)

(defstruct-generic Matrix ((rank t) (dtype keyword))
		   :slots (data)
		   :args  (&key (data nil)))

(declaim (ftype (function
		 ((<Matrix> 2 :float)
		  (<Matrix> 2 :float))
		 (<Matrix> 2 :float))
		matrix-fadd))
(defun matrix-fadd (x y)
  (declare (ignore x y))
  (make-matrix (2 :float)))

;; Should work
(defun any-ops ()
  (let ((a (make-matrix (2 :float) :data (make-mat-core `(10 10) :float)))
	(b (make-matrix (2 :float) :data (make-mat-core `(10 10) :float))))
    (matrix-fadd a b)))

;; Should produce errors:
(defun any-wrong-ops ()
  (let ((a (make-matrix (2 :double) :data (make-mat-core `(10 10) :float)))
	(b (make-matrix (2 :float) :data (make-mat-core `(10 10) :float))))
    (matrix-fadd a b)))

;; [TODO] Setting T -> Any Values are OK; the equivalent to (or (Matrix 1 :float) (Matrix 2 :float) ...)
(defmethod dispatch-by-dtype ((tensor #.(Matrix T :float)))
  
  )
