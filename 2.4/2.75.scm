
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
	  ((eq? op 'imag-part) (* r (sin a)))
	  ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  (else
	   (error "Unknown op - MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; Example
(define complex1 (make-from-mag-ang 12 (/ pi 4)))

(complex1 'magnitude) ; 12
(complex1 'angle)     ; 0.7853981633974483
(complex1 'real-part) ; 8.485281374238571
(complex1 'imag-part) ; 8.485281374238571

(sqrt (* 2 (sqr 8.485281374238571))) ; 12
