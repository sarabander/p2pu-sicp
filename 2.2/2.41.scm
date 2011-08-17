
(define (unique-triples n)
  (flatmap (λ (i) 
	      (flatmap (λ (j)
			  (map (λ (k)
				  (list k j i))
			       (enumerate-interval 1 (- j 1))))
		       (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(unique-triples 4) ; '((1 2 3) (1 2 4) (1 3 4) (2 3 4))

(define (triplesum triple)
  (+ (first  triple)
     (second triple)
     (third  triple)))

(define (triples-less-than-or-equal-to-n-summing-to-s n s)
  (filter (λ (triple) (= (triplesum triple) s))
	  (unique-triples n)))

(triples-less-than-or-equal-to-n-summing-to-s 9 12)
; '((3 4 5) (2 4 6) (1 5 6) (2 3 7) (1 4 7) (1 3 8) (1 2 9))
