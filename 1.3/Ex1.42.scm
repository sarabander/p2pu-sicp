(define (compose f g)
  (λ (x)
     (f (g x))))

(define (square x) (* x x))

((compose square inc) 6) ;=> 49

((compose sin sqrt) 7)
