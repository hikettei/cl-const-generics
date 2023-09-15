
(in-package :cl-const-generics)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest inputs)
    (intern (with-output-to-string (out) (dolist (sym inputs) (princ sym out)))))

  (defun ConstFixnum (name)
    (symb 'ConstantFixnum< name '>))

  ;; (defun ConstSymbol (value))
  )

(macrolet ((define-constant-fixnum (value)
	     `(eval-when (:compile-toplevel :load-toplevel :execute)
		(deftype ,(ConstFixnum value) ()
		  `(and (integer ,,value ,,value))))))
  #.`(progn
       ,@(loop for i fixnum upfrom 0 below 9 collect
	       `(define-constant-fixnum ,i))))

(defmacro define-constant-kit (name)
