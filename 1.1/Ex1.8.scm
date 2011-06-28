;; Exercise 1.8

(define (square x) (* x x))

(define (good-enough? guess next-guess)
  (< (abs (- next-guess guess)) 
     (* 1.0e-20 guess)))

;; Formula for successice improvement of guess y:
;;      x/y² + 2y
;;  y → ---------
;;          3

(define (improve guess x) 
  (/ (+ (/ x (square guess)) 
	(* 2 guess)) 
     3))

(define (cbrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (cbrt-iter (improve guess x)
		 x)))

;; Cube root 
(define (cbrt x)
  (cond ((< x 0) (- (cbrt-iter 1.0 (abs x))))
	((= x 0) 0)
	(else (cbrt-iter 1.0 x))))

(define cube (lambda (x) (* x x x)))

;; For testing cbrt in range 0..10 (works in Racket),
;; should return the input list:
(map (compose inexact->exact cbrt cube) 
     '(0 1 2 3 4 5 6 7 8 9 10))

;; works in MIT Scheme, which doesn't have compose:
(map inexact->exact 
     (map cbrt 
	  (map cube 
	       '(0 1 2 3 4 5 6 7 8 9 10))))

(inexact->exact 4.0) ; => 4

(cbrt 0)
(cbrt -8000)
(cbrt -0.000001)
(cbrt -7.23e+304)
(cbrt 4.81e-321)
(cube 10)

;; Does this work?       x/y² + y
;;                   y → --------
;;                          2

(define (improve guess x) 
  (/ (+ (/ x (square guess)) 
	(* 1 guess)) 
     2))

;; Seems to work, except with (cbrt -0.000001) which loops infinitely.

;; Reliable way to calculate nth roots:
;; http://en.wikipedia.org/wiki/Nth_root_algorithm
;; Our first iteration formula is based on this.
