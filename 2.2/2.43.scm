
(define (queens board-size)
  (define empty-board '())
  (define (adjoin-position new-row k rest-of-queens)
    (cons (list new-row k) rest-of-queens))
  (define (safe? k positions)
    (disjoint? (rays (car positions) board-size)
	       (cdr positions)))
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (new-row)
	    (map (lambda (rest-of-queens)
		   (adjoin-position new-row
				    k
				    rest-of-queens))
		 (queen-cols (- k 1))))
	  (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;; queen-cols is redundantly called 'board-size' times. That is big efficiency
;; sink. Eight queens puzzle takes about (8^7)T with this version, instead of
;; T of the original.
