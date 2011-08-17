
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate + 0 (map (λ (x) (if (not (pair? x))
				  1
				  (count-leaves x))) 
		       t)))

(count-leaves '(3 4 5 6)) ; 4
(count-leaves '(a (b (c)) ((d ((e)) f) g) (h i))) ; 9
