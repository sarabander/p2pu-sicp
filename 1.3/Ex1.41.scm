(define (inc x) (add1 x))

(define (double f)
  (λ (x)
     (f (f x))))

((double inc) 2) ; 4
((double sq) 81) ; 3
((double sqr) 4) ; 256

;; This should return 13:
(((double (double double)) inc) 5) ; => 21? 

;; What's really happening:

;; (double double) = (λ (x) (double (double x)))
;; (double (double double)) = (λ (x) ((double double) ((double double) x))) =
;; = (λ (x) (double (double (double (double x)))))

(((λ (x) (double (double (double (double x))))) inc) 5) =
((double (double (double (double inc)))) 5) 
;; we're incrementing by 16, hence 21
