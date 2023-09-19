
(in-package :cl-const-generics)

(defstruct-generic Tensor
    ((rank fixnum))
    :slots ((value 1) (dtype :float) (device t))
    :constructor (&key (value 0)))

(declaim (ftype (function ((<const> (Tensor 1))
			   (<const> (Tensor 2)))
			  (<const> (Tensor 3)))
		adds-rank))
(defun adds-rank (x y)
  (make-tensor (3) :value (+ (tensor-value x) (tensor-value y))))

(defun test-function ()
  (let ((x (make-tensor (1) :value 3))
	(y (make-tensor (2) :value 4)))
    (tensor-value (adds-rank x y))

    ;;(adds-rank x (adds-rank x y))
    ))


