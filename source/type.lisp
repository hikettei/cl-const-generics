
(in-package :cl-const-generics)

;; コンパイル前にTensor内で使う全てのShapeを記録+deftype
;; Inlining and optimizing;
;; structがValueを持つことなく実装したい;
;; struct作る + CLOS ClassをWrap + AbstractTensorを自然にWrap

;;
;; Fig:
;;  Top: Structure Tensor
;;          |_ Sub Structure Tensor GensymXXX ...
;;                      ...
;;           ^基本Sub XXXで計算 Structure Tensorを継承している
;;

;; Compiler-macroでstruct + struct -> structを返す関数を作る + 構造体を入れ替え

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *constructor-cache-table* (make-hash-table))
  (defun const-type-name (name &rest const)
    "Returns a function which returns a symbol like: Tensor(I<FIXNUM>=1, J<FIXNUM>=1)"
    #'(lambda (&rest args)
	(assert (= (length args) (length const))
		nil
		"const-type-name: Assertion failed because the length do not match: ~a and ~a" args const)
	
	(apply
	 #'symb
	 name
	 '|(|
	 (loop for c   in const
	       for arg in args
	       append
	       `(,(car c) ,(second c) = ,arg))
	 (list '|)|))))
  
  (defun deftype-blueprint-form (name const body struct-from
				 &aux
				   (f (apply #'const-type-name name const)))
    (declare (type symbol name))
    #'(lambda (&rest args)
	(let* ((type-specifier (apply f args))
	       (struct (symb-cache type-specifier)))
	  `(progn
	     ;; Find-symbolで代用
	     ;; ここで作るSymbolの名前をdefmethodでhogehoge...することで
	     ;; extensible typep
	     (when (null (find-symbol (symbol-name ',struct) (find-package :cl-const-generics.subtypes)))
	       (defstruct (,struct
			   (:constructor ,struct (,@(map 'list #'car const)))
			   (:include ,struct-from)))
	       
	       )
	     ',struct)))))

;; TODO DOC Error Check
(defmacro deftype-with-const (name (&rest const) &body body)
  "
## [macro] deftype-with-const
"

  (assert (every #'(lambda (form) (= 2 (length form))) const)
	  nil
	  "deftype-with-const: const=((name type) (name type)...)")
  
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defstruct (,name
		 (:constructor ,(symb 'make-original- name) (,@(map 'list #'car const))))
       ,@(loop for c in const
	       collect
	       (progn `(,(car c) ,(car c) :type ,(second c)))))

     (defmacro ,(symb 'make- name) (,@(map 'list #'car const))
       ;; [FIX]
       `(,(eval `(,',name ,,@(map 'list #'car const)))
	 ,,@(map 'list #'car const)))
     
     (defmacro ,name (,@(map 'list #'car const))
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (let ((deftype-form (deftype-blueprint-form ',name ',const ',body ',name)))
	   (let ((type-identifier (eval (apply deftype-form (list ,@(map 'list #'car const))))))
	     `',type-identifier))))))

(defmacro the! (const-value-type form)
  `(the ,(eval const-value-type) ,form))

;; rename: define-const-generic
;; ((name type pred)) t=ignore
(deftype-with-const Tensor
    ((rank fixnum))
  ;; body -> typepの条件に変えると思う
  `(or fixnum))

