
;; From the book
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
       (append (tree->list-1 (left-branch tree))
	       (cons (entry tree)
		     (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list
			     (right-branch tree)
			     result-list)))))
  (copy-to-list tree '()))

;; a.

;; The trees from figure 2.16
(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(tree->list-1 tree1) ; '(1 3 5 7 9 11)
(tree->list-2 tree1) ; '(1 3 5 7 9 11)

(tree->list-1 tree2) ; '(1 3 5 7 9 11)
(tree->list-2 tree2) ; '(1 3 5 7 9 11)

(tree->list-1 tree3) ; '(1 3 5 7 9 11)
(tree->list-2 tree3) ; '(1 3 5 7 9 11)

;; The 2 procedures seem to produce the same list with these trees.

;; We try some unbalanced trees:
(define tree4 '(1 () (2 () (3 () (4 () ())))))
(define tree5 '(1 (2 (3 (4 () ()) ()) ()) ()))

(tree->list-1 tree4) ; '(1 2 3 4)
(tree->list-2 tree4) ; '(1 2 3 4)

(tree->list-1 tree5) ; '(4 3 2 1)
(tree->list-2 tree5) ; '(4 3 2 1)

;; Still same results.

;; b.

(define counter 0)

;; Call counter added to both
(define (tree->list-1 tree)
  (set! counter (add1 counter))
  (if (null? tree)
      '()
       (append (tree->list-1 (left-branch tree))
	       (cons (entry tree)
		     (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (set! counter (add1 counter))
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list
			     (right-branch tree)
			     result-list)))))
  (copy-to-list tree '()))

(set! counter 0)

(tree->list-1 tree1) ; '(1 3 5 7 9 11)
(tree->list-2 tree1) ; '(1 3 5 7 9 11)

(tree->list-1 tree2) ; '(1 3 5 7 9 11)
(tree->list-2 tree2) ; '(1 3 5 7 9 11)

(tree->list-1 tree3) ; '(1 3 5 7 9 11)
(tree->list-2 tree3) ; '(1 3 5 7 9 11)

;; Each of the 6 procedures above took 13 steps to execute.

(tree->list-1 tree4) ; '(1 2 3 4)
(tree->list-2 tree4) ; '(1 2 3 4)

(tree->list-1 tree5) ; '(4 3 2 1)
(tree->list-2 tree5) ; '(4 3 2 1)

;; These took 9 steps.

(define tree6 '(7 (3 (1 () ()) (5 () ())) (9 () (13 (11 () ()) (15 () ())))))

(tree->list-1 tree6) ; '(1 3 5 7 9 11 13 15)
(tree->list-2 tree6) ; '(1 3 5 7 9 11 13 15)

;; Both of these took 17 steps.

;; So, both procedures take 2n + 1 steps to convert trees with n elements.
;; Order of growth is O(n).
