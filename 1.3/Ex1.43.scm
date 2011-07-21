(define (repeated f times)
  (λ (x)
     (define (again n)
     (if (zero? n)
	 x
	 (f (again (sub1 n)))))
     (again times)))

((repeated sqr 2) 5)

((repeated add1 5) 10)
