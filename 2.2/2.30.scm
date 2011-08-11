
;; First version
(define (square-tree tree)
  (cond ((null? tree) empty)
	((not (pair? tree)) (sqr tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

;; Second version with map
(define (square-tree tree)
  (map (λ (sub-tree)
	  (if (pair? sub-tree)
	      (square-tree sub-tree)
	      (sqr sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
