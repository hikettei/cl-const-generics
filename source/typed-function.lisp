
(in-package :cl-const-generics)


;; compiler-macroとThe!で型をUpdate

(defmacro invite-type (function-name)
  "
## [macro]
invite-type

Attributes the ConstGenerics to the function.
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (define-compiler-macro ',function-name (&whole form args)
       
       )))
