
(in-package :cl-user)

;;(load "cl-const-generics.asd")
;;(ql:quickload :cl-const-generics)

(defpackage :example
  (:use :cl :cl-const-generics))

(in-package :example)

;; [TODO] Runtime creation of generic

(defstruct-generic Matrix ((rank t) (dtype keyword))
		   :slots (vec)
		   :args  (&key (vec nil)))

(declaim (ftype (function
		 ((<Matrix> 2 :float)
		  (<Matrix> 2 :float))
		 (<Matrix> 2 :float))
		matrix-fadd))
(defun matrix-fadd (x y)
  (make-matrix (2 :float)
	       :vec (+ (matrix-vec x)
		       (matrix-vec y))))

(defun matrix-fsub-body (x y)
  (make-matrix (2 :float)
	       :vec (+ (matrix-vec x)
		       (matrix-vec y))))

;; [TODO] define-typed-function
(define-compiler-macro matrix-fsub (&rest vars &environment env)
  #+sbcl(print (multiple-value-list (sb-cltl2:function-information 'matrix-fsub env)))
  ;; It is possible to  Produce shape-error here
  (dolist (v vars)
    (print v)
    (when (symbolp v)
      (print (multiple-value-list (sb-cltl2:variable-information v env)))))
  `(the (<Matrix> 2 :float) (matrix-fsub1 ,@vars)))

(defun customized-error-any-ops ()
  (let ((a (make-matrix (2 :float) :vec 1))
	(b (make-matrix (2 :float) :vec 2)))
   ;; (declare (type (<Matrix> 2 :float) a b))
    ;; Without declare form..?
    (let ((o (matrix-fsub a b)))
      o)))

;; Should work
(defun any-ops ()
  (let ((a (make-matrix (2 :float) :vec 1))
	(b (make-matrix (2 :float) :vec 2)))
    (matrix-fadd a b)))

;; Should produce errors:
(defun any-wrong-ops ()
  (let ((a (make-matrix (2 :double) :vec 1))
	(b (make-matrix (2 :float)  :vec 2)))
    (matrix-fadd a b)))

;; [TODO] Setting T -> Any Values are OK; the equivalent to (or (Matrix 1 :float) (Matrix 2 :float) ...)
(defmethod dispatch-by-dtype ((tensor #.(Matrix T :float)))
  
  )
