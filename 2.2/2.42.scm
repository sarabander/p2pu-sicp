
;; From the book
(define (enumerate-interval low high)
  (if (> low high)
      empty
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))
;; --------------

(define nil empty)

(define (add0 x) x)

;; Produces a list of positions emanating like 3 rays from queen in center:
;; diagonally up, left and diagonally down:
;;
;;   *
;;     *
;;       *
;;   * * * Q
;;       *
;;     *
;;   *
;;
(define (rays center board-size)
  (define (ray center orientation)
    (let* ((rowstep (cond ((equal? orientation 'upstairs)   sub1)
			  ((equal? orientation 'leftward)   add0)
			  ((equal? orientation 'downstairs) add1)
			  (else 
			   (error 
			    "Must be 'upstairs, 'leftward or 'downstairs."))))
	   (rowindex (rowstep (car center)))
	   (columnindex (sub1 (cadr center)))
	   (out-of-board? (or (< rowindex 1)
			      (< columnindex 1)
			      (> rowindex board-size)
			      (> columnindex board-size))))
      (if out-of-board?
	  empty
	  (cons (list rowindex columnindex)
		(ray (list rowindex columnindex) orientation)))))
  (append (ray center 'upstairs)
	  (ray center 'leftward)
	  (ray center 'downstairs)))

(define (intersection set1 set2)
  (flatmap (λ (pos1) (filter (λ (pos2) (equal? pos1 pos2)) 
			     set2)) 
	   set1))

(define (disjoint? set1 set2)
  (empty? (intersection set1 set2)))

;; Unit tests
(rays '(4 7) 8)

(define queens1 '((3 1) (7 2) (2 3) (8 4) (5 5) (1 6)))
(define queens2 (reverse queens1))

(intersection queens2 (rays '(5 7) 8)) ; '((5 5) (8 4))

(empty? (intersection '(1 2 3 10) '(0 3 5))) ; #f

;; The main procedure. Position is a pair: (rowindex columnindex).
;; Here chessboard squares are enumerated from upper left corner (1 1). 
(define (queens board-size)
  (define empty-board '())
  (define (adjoin-position new-row k rest-of-queens)
    (cons (list new-row k) rest-of-queens))
  (define (safe? k positions) ; k is not used
    (disjoint? (rays (car positions) board-size)
	       (cdr positions)))
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

;; Tests
(queens 4) ; '(((3 4) (1 3) (4 2) (2 1)) ((2 4) (4 3) (1 2) (3 1))) (correct)
(queens 8) ; '((6 8) (4 7) (1 6) (5 5) (8 4) (2 3) (7 2) (3 1)) 
;; this is one solution from the generated sequence, same as in figure 2.8.

;; Number of solutions with increasing board sizes:
(length (queens 1))  ; 1
(length (queens 2))  ; 0
(length (queens 3))  ; 0
(length (queens 4))  ; 2
(length (queens 5))  ; 10
(length (queens 6))  ; 4
(length (queens 7))  ; 40
(length (queens 8))  ; 92
(length (queens 9))  ; 352
(length (queens 10)) ; 724

;; Appendix
;; Testing map and filter
(define (safe? k positions)
    (disjoint? (rays (car positions) 8)
	       (cdr positions)))

(filter
 (lambda (positions) (safe? 1 positions))
 (flatmap
  (lambda (rest-of-queens)
    (map (lambda (new-row)
	   (adjoin-position new-row
			    1
			    rest-of-queens))
	 (enumerate-interval 1 8)))
  '(())))

(filter
 (lambda (positions) (safe? 2 positions))
 (flatmap
  (lambda (rest-of-queens)
    (map (lambda (new-row)
	   (adjoin-position new-row
			    2
			    rest-of-queens))
	 (enumerate-interval 1 8)))
  '(((1 1)) ((2 1)) ((3 1)) ((4 1)) ((5 1)) ((6 1)) ((7 1)) ((8 1)))))
