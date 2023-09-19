
(in-package :cl-user)

(load "cl-const-generics.asd")
(ql:quickload :cl-const-generics)

(defpackage :example
  (:use :cl :cl-const-generics))

(in-package :example)

;(defclass Matrix-Core ()
;  (shape :initarg :shape :accessor shape-of)
;  (dtype :initarg :dtype :accessor dtype-of))

(defun make-mat-core (shape dtype)
  (make-instance 'Matrix-Core :shape shape :dtype dtype))

(defstruct-generic Matrix ((rank t) (dtype keyword))
		   :slots (data)
		   :args  (&key (data nil)))

;; The root; typepで色々分岐する
;; make-matrix ... うまく関数でできないか

(declaim (ftype (function
		 ((<const> (Matrix 2 :float))
		  (<const> (Matrix 2 :float)))
		 (<const> (Matrix 2 :float)))
		matrix-fadd))
(defun matrix-fadd (x y)
  (declare (ignore x y))
  (make-matrix (2 :float)))

(defun any-ops ()
  (let ((a (make-matrix (2 :float) :data (make-mat-core `(10 10) :float)))
	(b (make-matrix (2 :float) :data (make-mat-core `(10 10) :float))))
    (matrix-fadd a b)))

(defun any-wrong-ops ()
  (let ((a (make-matrix (2 :double) :data (make-mat-core `(10 10) :float)))
	(b (make-matrix (2 :float) :data (make-mat-core `(10 10) :float))))
    (matrix-fadd a b)))
