
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (if (< d 0)
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

(print-rat (make-rat 12 -15))
(print-rat (make-rat -4 -18))
(print-rat (make-rat -18 15))
(print-rat (make-rat 3 5))
