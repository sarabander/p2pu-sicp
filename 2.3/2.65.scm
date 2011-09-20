
;; Some helper procedures
(define (cut-left-branch tree)
  (make-tree (entry tree)
	     '()
	     (right-branch tree)))

(cut-left-branch (list->tree '(1 2 3 4 5))) ; '(3 () (4 () (5 () ())))

(define (cut-right-branch tree)
  (make-tree (entry tree)
	     (left-branch tree)
	     '()))

(cut-right-branch (list->tree '(1 2 3 4 5))) ; '(3 (1 () (2 () ())) ())

;; Union
(define (union-set-2 tree1 tree2)
  (set! counter (add1 counter))
  (cond ((empty? tree1) tree2)
	((empty? tree2) tree1)
	(else 
	 (let ((entry1 (entry tree1))
	       (entry2 (entry tree2))
	       (left1  (left-branch tree1))
	       (left2  (left-branch tree2))
	       (right1 (right-branch tree1))
	       (right2 (right-branch tree2)))
	   (cond ((= entry1 entry2)
		  (make-tree entry1 
			     (union-set-2 left1 left2)
			     (union-set-2 right1 right2)))
		 ((< entry1 entry2)
		  (union-set-2 
		   (make-tree entry2
			      (union-set-2 (cut-right-branch tree1)
					   left2)
			      right2)
		   right1))
		 ((< entry2 entry1)
		  (union-set-2 
		   (make-tree entry1
			      (union-set-2 (cut-right-branch tree2)
					   left1)
			      right1)
		   right2))
		 (else (error "Should be unreachable place.")))))))

;;Check
(tree->list-1 
 (union-set-2 '() 
	      (list->tree '(1 2 3)))) ; '(1 2 3)

(tree->list-1 
 (union-set-2 (list->tree '(1 2 3)) 
	      (list->tree '(1 2 3)))) ; '(1 2 3)

(tree->list-1 
 (union-set-2 (list->tree '(0 2 4)) 
	      (list->tree '(1 2 3)))) ; '(0 1 2 3 4)

(tree->list-1 
 (union-set-2 (list->tree '(-3 0 1)) 
	      (list->tree '(4 7 9)))) ; '(-3 0 1 4 7 9)

(list->tree
 (tree->list-1
  (union-set-2 (list->tree '(2 4 6 8)) 
	       (list->tree '(1 3 5))))) 
; '(4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (8 () ())))

(list->tree
 (tree->list-1
  (union-set-2 (list->tree '(2 4)) 
	       (list->tree '(1 3))))) ; counter = 28

(list->tree
 (tree->list-1
  (union-set-2 (list->tree '(2 4 6 8)) 
	       (list->tree '(1 3 5 7))))) ; counter = 52

(list->tree
 (tree->list-1
  (union-set-2 (list->tree '(2 4 6 8 10 12 14 16)) 
	       (list->tree '(1 3 5 7 9 11 13 15))))) ; counter = 100

(set! counter 0)

;; Growth of union-set-2 is linear.

;; Intersection
(define (intersection-set-2 tree1 tree2)
  (set! counter (add1 counter))
  (cond ((or (empty? tree1) (empty? tree2)) empty)
	(else 
	 (let ((entry1 (entry tree1))
	       (entry2 (entry tree2))
	       (left1  (left-branch tree1))
	       (left2  (left-branch tree2))
	       (right1 (right-branch tree1))
	       (right2 (right-branch tree2)))
	   (cond ((= entry1 entry2)
		  (make-tree entry1
			     (intersection-set-2 left1  left2)
			     (intersection-set-2 right1 right2)))
		 ((< entry1 entry2)
		  (union-set-2
		   (intersection-set-2 (cut-right-branch tree1)
				       left2)
		   (intersection-set-2 right1 tree2)))
		 ((< entry2 entry1)
		  (union-set-2
		   (intersection-set-2 (cut-right-branch tree2)
				       left1)
		   (intersection-set-2 right2 tree1)))
		 (else (error "Should be unreachable place.")))))))

(tree->list-1
 (intersection-set-2 (list->tree '()) 
		     (list->tree '()))) ; '()

(tree->list-1
 (intersection-set-2 (list->tree '(1 3 5 8 10)) 
		     (list->tree '(1 5 6 8 9)))) ; '(1 5 8)

(tree->list-1
 (intersection-set-2 (list->tree '(2 4 6 8)) 
		     (list->tree '(1 3 5 7)))) ; '()

;; Counting procedure calls
(set! counter 0)

(list->tree
 (tree->list-1
  (intersection-set-2 (list->tree '(2 4)) 
		      (list->tree '(1 3))))) ; counter = 22

(list->tree
 (tree->list-1
  (intersection-set-2 (list->tree '(2 4 6 8)) 
		      (list->tree '(1 3 5 7))))) ; counter = 42

(list->tree
 (tree->list-1
  (intersection-set-2 (list->tree '(2 4 6 8 10 12 14 16)) 
		      (list->tree '(1 3 5 7 9 11 13 15))))) ; counter = 91

(set! counter 0)

(list->tree
 (tree->list-1
  (intersection-set-2 (list->tree '(1 3)) 
		      (list->tree '(1 3))))) ; counter = 25

(list->tree
 (tree->list-1
  (intersection-set-2 (list->tree '(2 4 6 8)) 
		      (list->tree '(2 4 6 8))))) ; counter = 45

(list->tree
 (tree->list-1
  (intersection-set-2 (list->tree '(1 3 5 7 9 11 13 15)) 
		      (list->tree '(1 3 5 7 9 11 13 15))))) ; counter = 85

;; intersection-set-2 also grows linearly.
