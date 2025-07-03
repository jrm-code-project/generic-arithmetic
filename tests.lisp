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

#||
(test trigonometric
  (is (<= (abs (- (sin 0) 0)) 1e-10))
  (is (<= (abs (- (cos 0) 1)) 1e-10))
  (is (<= (abs (- (tan 0) 0)) 1e-10))
  (is (<= (abs (- (asin 0) 0)) 1e-10))
  (is (<= (abs (- (acos 1) 0)) 1e-10))
  (is (<= (abs (- (atan 0) 0)) 1e-10))
  (is (<= (abs (- (atan 1 1) (/ pi 4))) 1e-10)))

(test hyperbolic
  (is (<= (abs (- (sinh 0) 0)) 1e-10))
  (is (<= (abs (- (cosh 0) 1)) 1e-10))
  (is (<= (abs (- (tanh 0) 0)) 1e-10))
  (is (<= (abs (- (asinh 0) 0)) 1e-10))
  (is (<= (abs (- (acosh 1) 0)) 1e-10))
  (is (<= (abs (- (atanh 0) 0)) 1e-10)))

(test exp-log
  (is (<= (abs (- (exp 0) 1)) 1e-10))
  (is (<= (abs (- (log 1) 0)) 1e-10))
  (is (<= (abs (- (log (exp 2)) 2)) 1e-10)))
||#

(test expt-and-root
  (is (= (expt 2 3) 8))
  ;(is (= (expt 9 0.5) 3))
  ;(is (= (sqrt 16) 4))
  )

(test phase-cis
  (is (<= (abs (- (phase #C(0.0d0 1.0d0)) (/ pi 2))) 1e-10))
  (is (<= (abs (- (abs (cis 0.0d0)) 1.0d0)) 1e-10)))

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

;; To run all tests:
;; (fiveam:run! 'generic-arithmetic-suite) 