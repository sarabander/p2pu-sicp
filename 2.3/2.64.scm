;; Needs make-tree from 2.63

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (set! counter (add1 counter))
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree
				 (cdr non-left-elts)
				 right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree
		       this-entry left-tree right-tree)
		      remaining-elts))))))))

;; a.

;; In brief, partial-tree starts by partitioning the first n elements of 
;; list 'elts' into three parts: left part, node entry and right part. 
;; It then recursively calls partial-tree again on the left and right parts, 
;; producing left and right subtrees branching out from 'this-entry'. 
;; If n is odd, the left and right subtrees will be the same size, otherwise 
;; left is smaller. Finally, it uses make-tree to construct a tree out of 
;; the root entry ('this-entry'), left subtree and right subtree.

(list->tree '(1 3 5 7 9 11)) ; '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

;;         5
;;        / \
;;       /   \
;;      /     \
;;     1       9
;;      \     / \
;;       3   7  11

;; For checking
(tree->list-1 '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))))
;; '(1 3 5 7 9 11)

;; b.

;; The order of growth should be O(n), because in each step the list is halved, 
;; but two partial-tree calls are spawned. These halvings and doublings balance
;; each other out. We check this by adding a counter to partial-tree.

(define counter 0)

(list->tree '(1 3 5 7 9 11))             ; counter = 13
(list->tree '(1 3 5 7 9 11 13 15))       ; counter = 17
(list->tree '(1 3 5 7 9 11 13 15 17 19)) ; counter = 21

(set! counter 0)

;; Number of steps is 2n + 1, which is still O(n) growth.
