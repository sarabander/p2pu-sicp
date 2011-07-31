(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(lower-bound (make-interval 7.2 7.6))

(upper-bound (make-interval 7.2 7.6))
