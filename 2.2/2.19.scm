
;; From the book:

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

;; ---------------

(define (no-more? lst)
  (null? lst))

(define (first-denomination coinlist)
  (car coinlist))

(define (except-first-denomination coinlist)
  (cdr coinlist))

(cc 100 us-coins) ; 292
(cc 100 uk-coins) ; 104561

;; We reverse the order of coin lists
(define us-coins (reverse (list 50 25 10 5 1)))
(define uk-coins (reverse (list 100 50 20 10 5 2 1 0.5)))

(cc 100 us-coins) ; 292
(cc 100 uk-coins) ; 104561

;; Same results, but calculating with uk-coins takes considerably longer.

;; We now randomly change the ordering 
(define us-coins (list 10 50 1 5 25))
(define uk-coins (list 50 0.5 20 100 2 10 5 1))

(cc 100 us-coins) ; 292
(cc 100 uk-coins) ; 104561

;; Doesn't affect the result because of commutativity of addition.
;; In other words, when a node is the sum of its sub-branches, the ordering
;; of the branches doesn't matter.
