
(define (div-interval x y)
  (if (opposite-sign? (lower-bound y) (upper-bound y))
      (printf "I won't divide by an interval spanning zero.\n")
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

(define (opposite-sign? a b)
  (xor (negative? a) (negative? b)))

(define (xor x y)
  (or (and x (not y)) 
      (and y (not x))))

(div-interval (make-interval -8.3 -7.9)
	      (make-interval -2.3 1.4)) ; => error

(div-interval (make-interval -8.3 -7.9)
	      (make-interval 1.1 1.4)) ; => '(-7.55 . -5.64)
