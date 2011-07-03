;; Depends on 1.21 and 1.24 

(define (fermat-prime? n)
  (define (fast-prime n a)
    (cond ((= a 0) true)
	  ((fermat-test n a) (fast-prime n (- a 1)))
	  (else false)))
  (fast-prime n (- n 1)))

(define (fermat-test n a)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it a))

(map fermat-prime? '(561 1105 1729 2465 2821 6601)) ; Carmichael no-s fool it,
(map prime? '(561 1105 1729 2465 2821 6601))        ; but not this
