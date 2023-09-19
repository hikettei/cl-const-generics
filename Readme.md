
# cl-const-generics

This is an attempted to introduce [Const Generics](https://practice.rs/generics-traits/const-generics.html) to Common Lisp, but just take this as some kind of joke, not for practical. The purpose is to include this system into my project; cl-waffe2.

## Goals

- At least Useful for REPL, should be disabled anywhere anytime.

- The aim is to apply into tensor operations


## Usage

```lisp
(the #.(Tensor 1) (make-tensor 1))
(defmethod add ((a #.(tensor 1))) )
```

