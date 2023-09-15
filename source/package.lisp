
(in-package :cl-user)

(defpackage :cl-const-generics
  (:use :cl))

(in-package :cl-const-generics)

(defpackage :cl-const-generics.storeroom
  (:use :cl))

(defmacro define-generic-type (name (&rest args) &body body &aux (const (gensym "const")))
  "Types defined via this macro can receive <const> args"
  `(deftype ,name (,const ,@args)

     ;; [TODO] Parse keywords...
     (append `(and ,,@body)
	     (eval ,const))))

;; (defmacro define-generic-type
;; (defmacro define-const
;; (defmacro define-generic-type-obj-maker
;; AoT Shape-Error Check?

;; (defstruct ConstantType ()) ;; Fixnum Value
;; (defstruct GenericType ()) Type + List<Constants>

(defclass Tensor ()
  ((shape :initform nil :initarg :shape :type list)))

(defmethod print-object ((obj Tensor) stream)
  (format stream "<<Tensor: ~a>>" (slot-value obj 'shape)))

(define-generic-type RankedTensor ()
  `(and Tensor))  

(defmacro make-tensor (m n)
  `(the (RankedTensor (<const> :M ,m :N ,n))
	(make-instance 'tensor :shape (list ,m ,n))))

(defun storeroom-id () (intern (symbol-name (gensym "PRED")) 'cl-const-generics.storeroom))

(defmacro <const> (&key (m 1) (n 1))
  (eval-when (:compile-toplevel :load-toplevel :execute)
    `(list (typep (ConstFixnum ,m))
	   (typep (ConstFixnum ,n)))))

(declaim (ftype (function ((RankedTensor (<const> :M 1 :N 2))
			   (RankedTensor (<const> :M 1 :N 2)))
			  (RankedTensor  (<const> :M 1 :N 2)))
		matmul))
(defun matmul (a b)
  (declare (ignore a b))
  (let ((out (make-tensor 1 2)))
    out))

;; define-compiler-macro
;; Fast Enough, overheads are ignorable
(defun my-op ()
  (let ((out (matmul (make-tensor 1 2) (make-tensor 1 3))))
    (matmul out out)))

(defun my-op1 (a b)
  (declare (type (RankedTensor (<const> :M 1 :N 2)) a)
	   (type (RankedTensor (<const> :M 1 :N 2)) b))
  (matmul a b))

