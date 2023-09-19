
(in-package :cl-const-generics)

;; 1. Ranked を作ってみる
;; 2. Shaped を作ってみる
;; 3. Dtype比較を作ってみる

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest inputs)
    (intern (with-output-to-string (out) (dolist (sym inputs) (princ sym out)))))
  
  (defun symb-cache (&rest inputs)
    (intern (with-output-to-string (out) (dolist (sym inputs) (princ sym out))) (find-package :cl-const-generics.subtypes)))
  )
