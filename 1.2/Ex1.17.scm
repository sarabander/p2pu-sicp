(define (double x) (+ x x))

;;(define (halve x) (/ x 2)) ; if division is available

;; If division is not available:
(define (halve x)
  (define (halve-help y z)
    (if (= y (double z))
	((if (negative? x) - +) z)
	(halve-help y (- z 1))))
  (if (or (not (integer? x)) (odd? x))
      (printf "halve: argument must be even integer, given ~a.\n" x)
      (halve-help (abs x) (abs x))))

;;  XOR: Exclusive OR
;;
;;    x      y   (xor x y)
;;  ----------------------
;;  false  false  false
;;  false  true   true
;;  true   false  true
;;  true   true   false
;;  ----------------------
;;
(define (xor x y)
  (or (and x (not y)) 
      (and y (not x))))

(define (mult a b)
  (define (mult-help a b) ; a and b must be positive
    (cond ((or (zero? a) (zero? b)) 0)
	  ((= b 1) a)
	  ((even? b) (mult (double a) (halve b)))
	  ((odd?  b) (+ a (mult a (- b 1))))))
  ((if (xor (negative? a) (negative? b)) - +) 
   (mult-help (abs a) (abs b))))

(mult -6 7)
