# Generic Arithmetic for Common Lisp

This library re-implements the standard Common Lisp numeric functions as CLOS generic functions. This allows developers to easily extend the numeric tower with custom number types, making it possible to perform arithmetic operations on them in a natural and idiomatic way.

## Why Generic Arithmetic?

In standard Common Lisp, arithmetic functions like `+`, `-`, `*`, and `/` are not generic. This means they can only operate on the built-in number types (`integer`, `ratio`, `float`, `complex`). If you define a new number-like type, such as quaternions, dual numbers for automatic differentiation, or interval arithmetic, you cannot use the standard arithmetic operators on them directly. You would have to define new functions (`quaternion-+`, `dual-*`, etc.) which is cumbersome and un-lispy.

This library solves this problem by providing a set of generic functions that shadow the standard `cl` names. By implementing methods for these generic functions for your custom number types, you can seamlessly integrate them into the Lisp arithmetic system.

## Installation

This library is not yet in Quicklisp. To use it, clone this repository into your `~/quicklisp/local-projects/` directory:

```bash
git clone https://github.com/jrm-code-project/generic-arithmetic.git ~/quicklisp/local-projects/generic-arithmetic
```

Then, you can load it in your Lisp image:

```lisp
(ql:quickload :generic-arithmetic)
```

## Usage

To use the generic functions, you need to use the `generic-arithmetic` package. It's recommended to shadow the `cl` symbols.

```lisp
(defpackage #:my-new-numerics
  (:use #:cl #:generic-arithmetic))
```

### Example: Dual Numbers for Automatic Differentiation

Here is an example of how to define a new number type, `dual`, for forward-mode automatic differentiation. A dual number has two parts: a real part `u` and a dual part `u'`. The dual part represents the derivative.

The arithmetic rules for dual numbers are:
- `(u + u'ε) + (v + v'ε) = (u + v) + (u' + v')ε`
- `(u + u'ε) * (v + v'ε) = (uv) + (uv' + u'v)ε`

Here is how you can implement this with `generic-arithmetic`:

```lisp
(in-package #:my-new-numerics)

(defstruct (dual (:constructor dual (realpart deriv)))
  (realpart 0.0 :type double-float)
  (deriv 0.0 :type double-float))

(defmethod print-object ((d dual) stream)
  (format stream "#<~S u=~A, u'=~A>"
          (type-of d)
          (dual-realpart d)
          (dual-deriv d)))

;; Addition
(defmethod generic-arithmetic:add2 ((left dual) (right dual))
  (dual (+ (dual-realpart left) (dual-realpart right))
        (+ (dual-deriv left) (dual-deriv right))))

(defmethod generic-arithmetic:add2 ((left dual) (right number))
  (add2 left (dual right 0.0d0)))

(defmethod generic-arithmetic:add2 ((left number) (right dual))
  (add2 (dual left 0.0d0) right))

;; Multiplication
(defmethod generic-arithmetic:multiply2 ((left dual) (right dual))
  (dual (* (dual-realpart left) (dual-realpart right))
        (+ (* (dual-realpart left) (dual-deriv right))
           (* (dual-deriv left) (dual-realpart right)))))

(defmethod generic-arithmetic:multiply2 ((left dual) (right number))
  (multiply2 left (dual right 0.0d0)))

(defmethod generic-arithmetic:multiply2 ((left number) (right dual))
  (multiply2 (dual left 0.0d0) right))

;; Now you can use the standard operators
(let ((x (dual 5.0d0 1.0d0))) ; x = 5, dx/dx = 1
  (print (+ x 2))            ; => #<DUAL u=7.0, u'=1.0>
  (print (* x 3))            ; => #<DUAL u=15.0, u'=3.0>
  (print (* x x)))           ; => #<DUAL u=25.0, u'=10.0> (derivative of x^2 is 2x)
```

## Provided Generic Functions

This library provides generic versions of most of the standard Common Lisp numeric functions.

### Core Arithmetic
`+`, `-`, `*`, `/`, `1+`, `1-`

### Binary Operations
`add2`, `subtract2`, `multiply2`, `divide2`, `=2`, `max2`, `min2`

### Comparison
`<`, `<=`, `=`, `>`, `>=`, `/=`
`max`, `min`

### Unary Operations
`abs`, `negate`, `reciprocal`, `square`, `cube`, `sqrt`, `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh`, `signum`, `zerop`

### Type Conversions & Parts
`complex`, `realpart`, `imagpart`, `conjugate`, `numerator`, `denominator`, `rational`, `rationalize`

### Floating Point
`float`, `decode-float`, `integer-decode-float`, `scale-float`, `float-digits`, `float-precision`, `float-radix`, `float-sign`

### Rounding
`ceiling`, `floor`, `round`, `truncate`, `fceiling`, `ffloor`, `fround`, `ftruncate`

### Other
`cis`, `phase`, `mod`, `rem`, `random`

## Dependencies

- [fold](https://github.com/lisp-maintainers/fold): For folding over lists of numbers in n-ary operations.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
