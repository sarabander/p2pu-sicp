
(define (fringe tree)
  (cond ((null? tree) empty)
	((not (pair? tree)) (list tree))
	(else (append (fringe (car tree))
		      (fringe (cdr tree))))))

(fringe '((1 2 3) 4 ((5 6) 7 (8)) (9 (10 (11))))) 
; '(1 2 3 4 5 6 7 8 9 10 11)

(define x (list (list 1 2) (list 3 4)))

(fringe x) ; '(1 2 3 4)

(fringe (list x x)) ; '(1 2 3 4 1 2 3 4)
