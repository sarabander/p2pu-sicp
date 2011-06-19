;; Exercise 1.7

;; The ordinary good-enough? (Ex1.6) converges poorly with some numbers,
;; sometimes enters a long cycle. If the tolerance is very small 
;; (e.g. 1e-57), it converges when the exponent's absolute value is near 57.
;; With small exponents (like 10) it loops infinitely.

;; The new good-enough? seems to converge with all numbers from very small 
;; to very large. Tested in range 4e-324 to 1.7e+308, but the calculation
;; result is inaccurate near the lower bound. Upper limit of convergence
;; is near 1.7e+308 as it should be for double precision floating-point 
;; number. (http://en.wikipedia.org/wiki/Binary64#Double_precision_examples)

(define (good-enough? guess next-guess)
  (< (abs (- next-guess guess)) (* 1.0e-20 guess)))

(define (average x y) (/ (+ x y) 2))

(define (improve guess x) (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (sqrt x) (sqrt-iter 1.0 x))

(sqrt 16.0e-322)
(sqrt 81.0e-64)
(sqrt 4e-324) ; => 2.222e-162, should be 2e-162
(sqrt 6e-324) ; => 2.222e-162, should be 2.449e-162
(sqrt 1.7e+308) ; => 1.304e+154 is correct, but goes to infinite loop
; with larger numbers

(sqrt (+ (square 3) (square 4)))
