
;; We split the set so that set = subset1 + subset2. At first, subset1
;, is empty. Then we move elements from the start of subset2 to the
;; end of subset1 as if moving beads on the abacus. Every step we compare x 
;; to the first element of subset2. If it's less, we sandwich x between
;; subset1 and subset2. If it's equal, we throw it away and reunite subset1
;; with subset2. Average number of steps is n/2, where n is the number of 
;; elements in the set. Order of growth is still O(n).

(define (adjoin-set x set)
  (define (iter subset1 subset2)
    (cond ((or (empty? subset2) (< x (car subset2)))
	   (append subset1 (list x) subset2))
	  ((= x (car subset2))
	   (append subset1 subset2))
	  (else (iter (append subset1 
			      (list (car subset2)))
		      (cdr subset2)))))
  (iter empty set))

(adjoin-set 4  '(1 3 5 8)) ; '(1 3 4 5 8)
(adjoin-set 14 '(1 3 5 8)) ; '(1 3 5 8 14)
(adjoin-set 2  '(5 8))     ; '(2 5 8)
(adjoin-set 12 '())        ; '(12)
