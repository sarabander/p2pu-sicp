
;; From the book (searches linear sets)
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
	((equal? given-key (key (car set-of-records)))
	 (car set-of-records))
	(else (lookup given-key (cdr set-of-records)))))

;; --------------------------

(define alphabet-record
  (list->tree '((1 'a) (3 'c) (4 'd) (7 'g) (8 'h))))
; '((4 'd) ((1 'a) () ((3 'c) () ())) ((7 'g) () ((8 'h) () ())))

(define key car)

(key (entry alphabet-record)) ; 4

;; New lookup for binary trees
(define (lookup given-key tree-of-records)
  (cond ((null? tree-of-records) false)
	((= given-key (key (entry tree-of-records)))
	 (entry tree-of-records))
	((< given-key (key (entry tree-of-records)))
	 (lookup given-key (left-branch tree-of-records)))
	((> given-key (key (entry tree-of-records)))
	 (lookup given-key (right-branch tree-of-records)))
	(else (error "This location should be unreachable."))))


(lookup 7 alphabet-record) ; '(7 'g)
(lookup 1 alphabet-record) ; '(1 'a)
(lookup 4 alphabet-record) ; '(4 'd)
(lookup 2 alphabet-record) ; #f
(lookup 9 alphabet-record) ; #f
