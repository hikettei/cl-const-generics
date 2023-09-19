
(in-package :cl-const-generics)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Utils
  ;; When redefining structures defined by the structure which is defined by defstruct-generic
  ;; For most of processing systems; this behaviour is beyonds them.
  ;; So we no use to manually delete all old definitions
  (defun delete-subtype (structure-name)
    (let ((deflist (apropos-list structure-name (find-package *subtype-package-name*))))
      (dolist (def deflist)
	(setf (find-class def) nil))))
  
  (defun symb (&rest inputs)
    (intern (with-output-to-string (out) (dolist (sym inputs) (princ sym out)))))
  
  (defun symb-cache (&rest inputs)
    (intern (with-output-to-string (out) (dolist (sym inputs) (princ sym out))) (find-package *subtype-package-name*)))
  
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
    ;; [TODO] User defined translator for example:
    ;; :float is converted into :dense
    ;; :double is into :dense
    ;; Make :float and :double interoperatible
    #'(lambda (&rest args)
	(let* ((type-specifier (apply f args))
	       (struct (symb-cache type-specifier)))
	  `(progn
	     (when (null (ignore-errors (find-class ',struct)))
	       (let ((*package* (find-package *subtype-package-name*)))
		 (defstruct (,struct
			     (:constructor ,struct (,@(map 'list #'car const) ,@(get-params constructor-form)))
			     (:include ,struct-from)))))
	     ',struct)))))

(defmacro defstruct-generic (name (&rest const) &key (slots nil) (args nil))
  "
# [macro] defstruct-generic

```lisp
(defstruct-generic (name (&rest const) &key (slots nil) (args nil)))
```

Defines a structure of `name` but can use the feature of Const Generics. ~~As of this writing, the options regarding the structure defined here are intentionally kept to be limited; that should be kept small as it can be created at times unintentionally by the user. Therefore, the intended use is to wrap highly functional classes or structures with it.~~ (NVM about this)

This macro erases the old definition when it is compiled, so redefinition of the structure should work.

## Inputs

`name[symbol]` indicates a name of structure and generic-type indicator.

`const[list]` Const Generic Pamareters. Used to dispatch types

`slots[list]` a form indicating slots of the structure; never used to dispatch types.

`args[list]`  a form which placed after the `defstruct .. :constructor`.

## Effects

Plus, This macro defines two macros and a type in addition to the structure:

### [type] <name>

```lisp
(<Name> const)
```

Indicates the type considering `const`, and can be used like:

```lisp
(the (<Name> const) form)
```

### [macro] make-name

```lisp
(make-name (const) args)
```

This is the top of variables; creates the structure with given `const` and `args`, returning a structure with const. The arguments corresponding to `const` are evaluated in place, so no variables can be placed. On the other hand, `args` are used to create a new constructor and evaluations are done after the expansion.

### [macro] name

```lisp
(name const)
```

This macro is not only indicating `type-specifier` of const generics, but telling the combination of `const` used the toplevel. Accordingly, it always should be placed where evaluation should be done before the execution. For example, there's several ways of representing Const Generics Types:

```lisp
;; Using Type Indicator
(the (<Name> 1 2) form)

;; Macro + Evaluating
(the #.(Name 1 2) form)

;; Method Dispatching

(defmethod any-method ((x #.(Name 1 2)))
    body)

;; Ftype declaration

(declaim (ftype (function (<Name> 1 2) (<Name> 3 4)) my-function))
```

See also: examples.lisp
"

  (assert (every #'(lambda (form) (= 2 (length form))) const)
	  nil
	  "deftype-with-const: const=((name type) (name type)...)")

  (let ((constructor args))
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

       (deftype ,(symb '< name '>) (,@(map 'list #'car const))
	 `(and ,(eval `(,',name ,,@(map 'list #'car const)))))

       (defmacro ,(symb 'make- name) ((,@(map 'list #'car const)) ,@constructor)
	 `(,(eval `(,',name ,,@(map 'list #'car const)))
	   ,,@(map 'list #'car const)
	   ,,@(get-params constructor)))
       
       (defmacro ,name (,@(map 'list #'car const))
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (let ((deftype-form (deftype-blueprint-form ',name ',const ',name ',constructor)))
	     (let ((type-identifier (eval (apply deftype-form (list ,@(map 'list #'car const))))))
	       `',type-identifier)))))))


