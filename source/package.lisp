
(in-package :cl-user)

(defpackage :cl-const-generics
  (:use :cl)
  (:export
   #:defstruct-generic
   #:<const>))

(in-package :cl-const-generics)

(eval-when (:compile-toplevel :execute :load-toplevel)
  #.(progn
      (defparameter *subtype-package-name* (intern (symbol-name (gensym)) "KEYWORD"))
      (eval `(defpackage ,*subtype-package-name*))))

