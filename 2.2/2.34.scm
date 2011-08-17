
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (λ (this-coeff higher-terms) 
		 (+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))
