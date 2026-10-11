;;;; tests.lisp -- FiveAM tests for generic-arithmetic

(defpackage "GENERIC-ARITHMETIC-TESTS"
  (:shadowing-import-from "GENERIC-ARITHMETIC"
    "+" "-" "*" "/" "<" "<=" "=" ">" ">=" "/="
    "1+" "1-"
    "ABS" "ACOS" "ACOSH" "ADD2" "ASIN" "ASINH" "ATAN" "ATANH" "CEILING" "CIS" "CONJUGATE" "COS" "COSH" "CUBE" "DENOMINATOR" "DIVIDE2"
    "EXP" "EXPT" "FLOOR" "IMAGPART" "LOG" "MAX" "MAX2" "MIN" "MIN2" "MOD" "MULTIPLY2" "NEGATE" "NUMERATOR" "PHASE" "RANDOM" "RATIONALIZE" "REALPART"
    "RECIPROCAL" "REM" "ROUND" "SCALE-FLOAT" "SIGNUM" "SIN" "SINH" "SQRT" "SQUARE" "SUBTRACT2" "TAN" "TANH" "TRUNCATE" "ZEROP")
  (:use "CL" "FIVEAM" "GENERIC-ARITHMETIC"))

(in-package "GENERIC-ARITHMETIC-TESTS")

(def-suite generic-arithmetic-suite)

(in-suite generic-arithmetic-suite)

(test addition
  (is (= (+ 1 2 3) 6))
  (is (= (+ 1.5 2.5) 4.0))
  (is (= (+) 0))
  (is (= (+ 42) 42)))

(test subtraction
  (is (= (- 10 3 2) 5))
  (is (= (- 10) -10))
  (is (= (- 5.5 2.5) 3.0)))

(test multiplication
  (is (= (* 2 3 4) 24))
  (is (= (* 1.5 2) 3.0))
  (is (= (*) 1))
  (is (= (* 7) 7)))

(test division
  (is (= (/ 8 2 2) 2))
  (is (= (/ 8) 1/8))
  (is (= (/ 9 3) 3)))

(test comparisons
  (is (< 1 2 3))
  (is (<= 1 1 2))
  (is (= 2 2 2))
  (is (> 3 2 1))
  (is (>= 3 3 2))
  (is (/= 1 2 3)))

(test unary-ops
  (is (= (abs -5) 5))
  (is (= (abs 5) 5))
  (is (= (negate 7) -7))
  (is (= (reciprocal 4) 1/4))
  (is (= (square 3) 9))
  (is (= (cube 2) 8)))

(test rounding
  (is (= (ceiling 2.3) 3))
  (is (= (floor 2.7) 2))
  (is (= (round 2.5) 2))
  (is (= (truncate 2.7) 2)))

(test min-max
  (is (= (min 1 2 3) 1))
  (is (= (max 1 2 3) 3)))

(test type-conversions
  (is (= (realpart #C(2 3)) 2))
  (is (= (imagpart #C(2 3)) 3))
  (is (= (conjugate #C(2 3)) #C(2 -3))))

(test signum-zerop
  (is (= (signum 5) 1))
  (is (= (signum -5) -1))
  (is (zerop 0))
  (is (not (zerop 1))))

(test trigonometric
  (is (<= (abs (- (sin 0) 0)) 1d-6))
  (is (<= (abs (- (cos 0) 1)) 1d-6))
  (is (<= (abs (- (tan 0) 0)) 1d-6))
  (is (<= (abs (- (asin 0) 0)) 1d-6))
  (is (<= (abs (- (acos 1) 0)) 1d-6))
  (is (<= (abs (- (atan 0) 0)) 1d-6))
  (is (<= (abs (- (atan 1 1) (/ pi 4))) 1d-6)))

(test hyperbolic
  (is (<= (abs (- (sinh 0) 0)) 1d-6))
  (is (<= (abs (- (cosh 0) 1)) 1d-6))
  (is (<= (abs (- (tanh 0) 0)) 1d-6))
  (is (<= (abs (- (asinh 0) 0)) 1d-6))
  (is (<= (abs (- (acosh 1) 0)) 1d-6))
  (is (<= (abs (- (atanh 0) 0)) 1d-6)))

(test exp-log
  (is (<= (abs (- (exp 0) 1)) 1d-6))
  (is (<= (abs (- (log 1) 0)) 1d-6))
  (is (<= (abs (- (log (exp 2)) 2)) 1d-6)))
(test expt-and-root
  (is (= (expt 2 3) 8))
  (is (= (expt 2 10) 1024))
  (is (= (expt 2 -2) 1/4))
  (is (= (expt 5 0) 1))
  (is (= (expt 5 1) 5))
  (is (= (expt 0 5) 0))
  (is (= (expt 1 5) 1))
  (is (<= (cl:abs (cl:- (expt 9 0.5d0) 3)) 1d-10))
  (is (= (sqrt 16) 4))
  (is (<= (cl:abs (cl:- (sqrt 2d0) (cl:sqrt 2d0))) 1d-12)))

(test phase-cis
  (is (<= (abs (- (phase #C(0.0d0 1.0d0)) (/ pi 2))) 1d-6))
  (is (<= (abs (- (abs (cis 0.0d0)) 1.0d0)) 1d-6)))

(test numerator-denominator
  (is (= (numerator 3/4) 3))
  (is (= (denominator 3/4) 4)))

(test mod-rem
  (is (= (mod 10 3) 1))
  (is (= (rem 10 3) 1))
  (is (= (mod -10 3) 2))
  (is (= (rem -10 3) -1)))

(test random
  (let ((r (random 10)))
    (is (and (integerp r) (<= 0 r 9)))))

(test edge-cases
  (is (= (add2 0 5) 5))
  (is (= (add2 5 0) 5))
  (is (= (multiply2 1 7) 7))
  (is (= (multiply2 7 1) 7))
  (is (= (multiply2 0 7) 0))
  (is (= (multiply2 7 0) 0))
  (is (= (divide2 1 2) 1/2))
  (is (= (divide2 2 1) 2))
  (is (= (subtract2 0 5) -5))
  (is (= (subtract2 5 0) 5)))

(test identity-shortcuts
  (is (= (add2 0 5) 5))
  (is (= (add2 5 0) 5))
  (is (eql (add2 0.0 5) 5.0))
  (is (eql (add2 5 0.0d0) 5.0d0))
  (is (= (subtract2 7 3) 4))
  (is (eql (subtract2 5 0.0) 5.0))
  (is (= (divide2 6 3) 2))
  (is (= (divide2 1 4) 1/4))
  (is (= (multiply2 3 4) 12))
  (is (eql (multiply2 0.0 5) 0.0))
  (is (eql (multiply2 5 1.0) 5.0))
  (is (eql (multiply2 5 1.0d0) 5.0d0)))

(test nary-edge-cases
  (is (= (+ 1 2 3 4 5) 15))
  (is (= (* 1 2 3 4 5) 120))
  (is (= (- 10 1 2 3) 4))
  (is (= (/ 24 2 3) 4))
  (is (= (max 5) 5))
  (is (= (min 5) 5))
  (is (= (max 3 9 2 7) 9))
  (is (= (min 3 9 2 7) 2)))

(test comparison-failures
  (is (not (< 1 1)))
  (is (not (< 3 2 1)))
  (is (not (<= 2 1)))
  (is (not (= 1 2)))
  (is (not (= 1 1 2)))
  (is (not (> 1 2)))
  (is (not (>= 1 2)))
  (is (not (/= 1 2 1)))
  (is (/= 1 2))
  (is (< 1 2))
  (is (< 1 1.5 2)))

(test one-plus-one-minus
  (is (= (1+ 5) 6))
  (is (= (1- 5) 4))
  (is (= (1+ 1/2) 3/2))
  (is (= (1- 1.5d0) 0.5d0)))

(test rounding-with-divisor
  (is (equal (multiple-value-list (floor 7 2)) '(3 1)))
  (is (equal (multiple-value-list (ceiling 7 2)) '(4 -1)))
  (is (equal (multiple-value-list (truncate -7 2)) '(-3 -1)))
  (is (equal (multiple-value-list (round 7 2)) '(4 -1)))
  (is (= (floor -2.5) -3))
  (is (= (ceiling -2.5) -2)))

(test log-and-exp
  (is (= (log 1) 0))
  (is (eql (log 1.0d0) 0.0d0))
  (is (= (exp 0) 1))
  (is (<= (cl:abs (cl:- (log 8 2) 3)) 1d-10))
  (is (<= (cl:abs (cl:- (log 100 10) 2)) 1d-10))
  (is (<= (cl:abs (cl:- (exp 1d0) cl:pi)) 1)))

(test atan-two-arg
  (is (<= (cl:abs (cl:- (atan 1d0 1d0) (cl:/ cl:pi 4))) 1d-10))
  (is (<= (cl:abs (cl:- (atan 1d0) (cl:atan 1d0))) 1d-12)))

(test inverse-pairs
  (is (eq (generic-arithmetic::inverse 'sin) 'asin))
  (is (eq (generic-arithmetic::inverse 'cos) 'acos))
  (is (eq (generic-arithmetic::inverse 'sinh) 'asinh))
  (is (eq (generic-arithmetic::inverse 'cosh) 'acosh))
  (is (eq (generic-arithmetic::inverse 'exp) 'log))
  (is (eq (generic-arithmetic::inverse 'log) 'exp))
  (is (eq (generic-arithmetic::inverse 'negate) 'negate))
  (is (eq (generic-arithmetic::inverse 'reciprocal) 'reciprocal))
  (is (eq (generic-arithmetic::inverse 'square) 'sqrt))
  (is (eq (generic-arithmetic::inverse 'sqrt) 'square)))

(test regressions
  (is (eql (add2 5 0.0d0) 5.0d0))
  (is (eql (subtract2 5 0.0d0) 5.0d0))
  (is (eql (multiply2 1.0d0 5) 5.0d0))
  (is (eql (log 2.0) (cl:log 2.0)))
  (is (< 1))
  (is (= 1))
  (is (eq (generic-arithmetic::inverse '1+) '1-))
  (is (eq (generic-arithmetic::inverse '1-) '1+))
  (is (eq (generic-arithmetic::inverse #'1+) #'1-))
  (is (eq (generic-arithmetic::inverse #'1-) #'1+)))

;;; Extensibility: a user-defined number-like type.

(defstruct (dual (:constructor dual (realpart* deriv)))
  (realpart* 0 :type real)
  (deriv 0 :type real))

(defmethod add2 ((left dual) (right dual))
  (dual (+ (dual-realpart* left) (dual-realpart* right))
        (+ (dual-deriv left) (dual-deriv right))))

(defmethod add2 ((left dual) (right real))
  (dual (+ (dual-realpart* left) right) (dual-deriv left)))

(defmethod add2 ((left real) (right dual))
  (add2 right left))

(defmethod multiply2 ((left dual) (right dual))
  (dual (* (dual-realpart* left) (dual-realpart* right))
        (+ (* (dual-realpart* left) (dual-deriv right))
           (* (dual-deriv left) (dual-realpart* right)))))

(defmethod multiply2 ((left dual) (right real))
  (dual (* (dual-realpart* left) right) (* (dual-deriv left) right)))

(defmethod multiply2 ((left real) (right dual))
  (multiply2 right left))

(defmethod negate ((d dual))
  (dual (- (dual-realpart* d)) (- (dual-deriv d))))

(defun dual= (d r p)
  (and (dual-p d)
       (cl:= (dual-realpart* d) r)
       (cl:= (dual-deriv d) p)))

(test user-defined-type
  (let ((a (dual 2 1))
        (b (dual 3 4)))
    (is (dual= (+ a b) 5 5))
    (is (dual= (+ a b a) 7 6))
    (is (dual= (+ a 10) 12 1))
    (is (dual= (+ 10 a) 12 1))
    (is (dual= (* a b) 6 11))
    (is (dual= (* 3 a) 6 3))
    (is (dual= (negate a) -2 -1))
    (is (dual= (- a) -2 -1))
    (is (dual= (- b a) 1 3))
    (is (dual= (square a) 4 4))
    (is (dual= (cube a) 8 12))
    (is (dual= (expt a 3) 8 12))))

(test no-method-errors
  (signals error (add2 :a :b))
  (signals error (negate :a))
  (signals error (sin "x")))

;; To run all tests:
;; (fiveam:run! 'generic-arithmetic-suite)
