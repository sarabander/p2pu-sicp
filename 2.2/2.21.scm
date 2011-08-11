
(define (square-list items)
  (if (null? items)
      empty
      (cons (sqr (car items)) (square-list (cdr items)) )))

(define (square-list items)
  (map sqr items))

(square-list '(10 9 8 7 6))
