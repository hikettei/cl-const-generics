
(in-package :cl-const-generics)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun delete-subtype (structure-name)
    (let ((deflist (apropos-list structure-name (find-package :cl-const-generics.subtypes))))
      (dolist (def deflist)
	(setf (find-class def) nil))))
  
  (defun symb (&rest inputs)
    (intern (with-output-to-string (out) (dolist (sym inputs) (princ sym out)))))
  
  (defun symb-cache (&rest inputs)
    (intern (with-output-to-string (out) (dolist (sym inputs) (princ sym out))) (find-package :cl-const-generics.subtypes)))
  
  (defun env-parameter-p (sym) (equal (aref (symbol-name sym) 0) #\&))
  (defun get-params (list)
    "Reading the given list, which is a form of arguments with defun, the function returns a list of symbols"
    (delete-duplicates
     (loop for i fixnum upfrom 0 below (length list)
	   append
	   (let ((sym (nth i list)))
	     (typecase sym
	       (symbol
		(if (env-parameter-p sym)
		    nil
		    (list sym)))
	       (list
		(if (= (length sym) 2)
		    (list (car sym))
		    (get-params sym))))))))
  
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
  
  (defun deftype-blueprint-form (name const struct-from constructor-form
				 &aux
				   (f (apply #'const-type-name name const)))
    (declare (type symbol name))
    #'(lambda (&rest args)
	(let* ((type-specifier (apply f args))
	       (struct (symb-cache type-specifier)))
	  `(progn
	     (when (null (ignore-errors (find-class ',struct)))
	       (let ((*package* (find-package :cl-const-generics.subtypes)))
		 (defstruct (,struct
			     (:constructor ,struct (,@(map 'list #'car const) ,@(get-params constructor-form)))
			     (:include ,struct-from)))))
	     ',struct)))))

;; TODO DOC Error Check
;; TODO Checking Form
;; TODO Extending Typep Forms
(defmacro defstruct-generic (name (&rest const) &key (slots nil) (constructor nil))
  "
## [macro] defstruct-generic

const ... (name type ?)
constructor ... must be &key (a 1) (b 1)
"

  (assert (every #'(lambda (form) (= 2 (length form))) const)
	  nil
	  "deftype-with-const: const=((name type) (name type)...)")
  
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (delete-subtype (symbol-name ',name))
     (defstruct (,name
		 (:constructor
		     ,(symb 'make-original- name)
		     (,@(map 'list #'car const) ,@(get-params constructor))))
       ,@(loop for c in const
	       collect
	       (progn `(,(car c) ,(car c) :type ,(second c))))
       ,@slots)

     (defmacro ,(symb 'make- name) ((,@(map 'list #'car const)) ,@constructor)
       `(,(eval `(,',name ,,@(map 'list #'car const)))
	 ,,@(map 'list #'car const)
	 ,,@(get-params constructor)))
     
     (defmacro ,name (,@(map 'list #'car const))
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (let ((deftype-form (deftype-blueprint-form ',name ',const ',name ',constructor)))
	   (let ((type-identifier (eval (apply deftype-form (list ,@(map 'list #'car const))))))
	     `',type-identifier))))))

(deftype <const> (form) "" `(and ,(eval form)))

