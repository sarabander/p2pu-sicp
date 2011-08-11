
(define nil empty)

(define square sqr)

;; From the book
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items nil))

(square-list '(1 2 3 4 5 6)) ; '(36 25 16 9 4 1)

;; Order is reversed because cons grows the list from left. 

;; Fix attempt from the book
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items nil))

(square-list '(1 2 3 4 5 6)) ; '((((((() . 1) . 4) . 9) . 16) . 25) . 36)

;; The first time cons is called, answer is an empty list. Cons makes
;; a pair from '() and 1 producing '(() . 1). This in turn will be the car
;; of a new pair constructed during next iteration: '((() . 1) . 4), etc.
