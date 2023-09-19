
(in-package :cl-user)

(defpackage :cl-const-generics-asd
  (:use :cl :asdf :uiop))

(in-package :cl-const-generics-asd)

(defsystem :cl-const-generics
  :author "hikettei"
  :licence "MIT"
  :description "An attempt to introduce Const Generics to Common Lisp"
  :pathname "source"
  :serial t
  :components ((:file "package")
	       (:file "cl-const-generics")))


