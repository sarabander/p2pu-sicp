
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

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

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rat n d) 
  (let ((g (gcd n d))
	(sign (if (xor (positive? n) 
		       (positive? d)) 
		  - 
		  +)))
    (cons (/ (sign (abs n)) g) 
	  (/ (abs d) g))))

(print-rat (make-rat 12 -15))
(print-rat (make-rat -4 -18))
(print-rat (make-rat -18 15))
(print-rat (make-rat 3 5))
