
# cl-const-generics

> ⚠️ There is no theoretical guarantee that the approach taken in this project will work without no problems. I do not recommend to use this for your project; this is almost for fun.

This is a meta-programming attempt to introduce the feature of [Const Generics in Rust](https://practice.rs/generics-traits/const-generics.html) to Common Lisp. The most main objective is to achieve pre-compilation error detection in [my tensor library](https://github.com/hikettei/cl-waffe2) by using structures with const generic parameters and inspecting their dtypes/ranks/dimensions. In the first place, Common Lisp is dynamically typed language and its ecosystem do not allow us to extend [Compound Types](http://clhs.lisp.se/Body/01_ddfa.htm). CLOS Classes can only identify themselve by nothing but symbol. To consider certain constants, we have to redefine the class. This small library is intended to solve the problem by introducing the `parameters` in addition to `type-specifier`. For example, Let's define a structure with its dtype and rank;

```lisp
(defstruct-generic Matrix ((rank t) (dtype keyword))
		   :slots (data)
		   :args  (&key (vec nil)))
```

The macro `defstruct-generic` defines a structure with const generic parameters: `rank` and `dtype`. `vec` is just a storage vector and never used to dispatch types. This structure can be transformed into another types depending on the parameter:

```lisp
(the (Matrix 3 :float) (make-matrix (3 :float) :vec (make-array `(3 3 3))))

(typep (make-matrix (3 :float) :vec nil) (Matrix 3 :float))

(declaim (ftype (function
		 ((<Matrix> 2 :float) (<Matrix> 2 :float)) (<Matrix> 2 :float))
		matrix-fadd))
(defun matrix-fadd (x y)
  (declare (ignore x y))
  (make-matrix (2 :float)))

;; Everything is ok, should work
(defun any-ops ()
  (let ((a (make-matrix (2 :float) :data (make-mat-core `(10 10) :float)))
	(b (make-matrix (2 :float) :data (make-mat-core `(10 10) :float))))
    (matrix-fadd a b)))

;; :dtype do not match
;; The moment this function is compiled with C-c C-c
;; also produces an error simultaneously.
(defun any-wrong-ops ()
  (let ((a (make-matrix (2 :double) :data (make-mat-core `(10 10) :float)))
	(b (make-matrix (2 :float) :data (make-mat-core `(10 10) :float))))
    (matrix-fadd a b)))

;; Should be used to dispatch methods
;; But the T option is not yet available.
(defmethod dispatch-by-dtype ((tensor #.(Matrix T :float)))
    body
    ...
    )
```

## How cl-const-generics work?

The primaly strategy is that reading type-specifiers with parameters before compiling the function, and defines a new structures which is a subtype of original structure. With proper inlining, it has little or no impact on execution time (the additional cost = wrapping the structure) and I except fasl to cache it when compiling the project as well. In the large scale, this approach should work as long as all combinations to be used as parameters are known before compilation.

## Goals

- Completely zero effects on a execution time.

- `typep` is user extensible. and `T` match everything.

- Just one line is need to wrap existing CLOS class. and no other code rewriting is never needed.

- AOT Shape Error of matrix operations. Ultimately, Merge into cl-waffe2.

# Usage

Very simple to use because `defstruct-generic` is the only macro exported from `:cl-const-generics` package. Just using this macro instead of `defstruct`.

# [macro] defstruct-generic

```lisp
(defstruct-generic (name (&rest const) &key (slots nil) (args nil)))
```

Defines a structure of `name` but can use the feature of Const Generics. ~~As of this writing, the options regarding the structure defined here are intentionally kept to be limited; that should be kept small as it can be created at times unintentionally by the user. Therefore, the intended use is to wrap highly functional classes or structures with it.~~ (NVM about this.)

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

