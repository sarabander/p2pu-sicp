
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map (λ (set) 
			     (cons (car s) set)) 
			  rest)))))

(subsets '(1 2 3)) ; '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;; It works, because the subsets of a set can be divided into two groups:
;; subsets that don't contain the first element of the set, and subsets
;; that do. But the latter group is just the first group of subsets where 
;; the first element of the set is added to each group member.

;; Here, with set '(1 2 3), we first take away 1 and find all the subsets
;; of '(2 3). They are: (), (3), (2) and (2 3). From these four subsets
;; we make additional four subsets by including 1 in each:
;; (1), (1 3), (1 2) and (1 2 3). We now have a set of eight sets that are 
;; the subsets of '(1 2 3): '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)).
