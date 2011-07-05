;; Depends on 1.20, 1.21, 1.28

(define (filtered-accumulate filter 
			     combiner 
			     null-value 
			     term 
			     a 
			     next 
			     b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) 
	      (combiner (if (filter a)
			    (term a)
			    null-value)
			result))))
  (iter a null-value))

;; test
(+ (filtered-accumulate odd? + 0 identity 1 inc 10)
   (filtered-accumulate even? + 0 identity 1 inc 10)) ; 55

(+ (filtered-accumulate odd? + 0 cube 1 inc 10)
   (filtered-accumulate even? + 0 cube 1 inc 10)) ; 3025

(apply + (map cube '(1 2 3 4 5 6 7 8 9 10))) ; 3025

;; a.

;; input are primes between 2 and 50 (result for reference point)
(apply + (map square '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47))) ; 10466

;; with exhaustive prime test
(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

;; with Miller-Rabin probabilistic test
(define (sum-prime-squares a b)
  (filtered-accumulate miller-rabin-prime? + 0 square a inc b))

(sum-prime-squares 2 50) ; 10466 (matches the previous result)

;; b.

(define rel-prime?
  (λ (n)
     (λ (i)
	(= (gcd i n) 1))))

((rel-prime? 20) 9) ; #t

(define (prod-rel-prime n)
  (filtered-accumulate (rel-prime? n) * 1 identity 1 inc (- n 1)))

(prod-rel-prime 9)  ; 2240
(prod-rel-prime 10) ; 189
(prod-rel-prime 11) ; 3628800
