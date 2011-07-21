(define (iterative-improve good? make-better)
  (λ (y)
     (define (iter y)
       (if (good? y)
	   y
	   (iter (make-better y))))
     (iter y)))

(define (average a b)
  (/ (+ a b) 2))

(define (sq-root x)
  (let* ((tolerance 0.0001)
	 (good-enough? (λ (guess)
			  (< (abs (- x (sqr guess))) tolerance)))
	 (improve (λ (guess)
		     (average guess (/ x guess)))))
    ((iterative-improve good-enough? improve) 1.0)))

(sq-root 729) ; 27

(define (fixed-point f guess)
  (let* ((tolerance 0.000001)
	 (good-enough? (λ (guess)
			  (< (abs (- guess (f guess))) tolerance)))
	 (improve (λ (guess)
		     (average guess (f guess)))))
    ((iterative-improve good-enough? improve) guess)))

(fixed-point cos 1.0) ; 0.7391
(fixed-point (λ (x) (/ 49 x)) 1.0) ; 7
