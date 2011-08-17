
(define (reverse sequence)
  (fold-right (λ (x y) (append y (list x)))
	      nil 
	      sequence))

(define (reverse sequence)
  (fold-left (λ (x y) (cons y x)) 
	     nil 
	     sequence))

(reverse '(1 2 3 4))
(reverse '(a b c))
