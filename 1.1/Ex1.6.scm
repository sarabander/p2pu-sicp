;; Exercise 1.6

;; Helper procedures to test new sqrt-iter:

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 1.0e-57))

(define (average x y) (/ (+ x y) 2))

(define (improve guess x) (average guess (/ x guess)))

(define (sqrt x) (sqrt-iter 1.0 x))

;; Our dubious new construct:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

;; new-if is not a special form - therefore it always evaluates then-clause
;; and else-clause regardless of the predicate value.
;; The following procedure loops forever and eats memory fast:

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
		     x)))

;; You can't detect problems using simple cases like this:
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

;; Goes to never-ending recursion:

(sqrt (+ (square 3) (square 4)))

;; Ctrl-C twice for breaking out

;; sqrt-iter with proper if restored:

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

;; Now it works:

(sqrt (+ (square 3) (square 4)))
