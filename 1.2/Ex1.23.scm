;; Depends on 1.21 and 1.22

;; modified from 1.21 to use next
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (next input)
  (if (= input 2)
      3
      (+ 2 input)))

(search-for-primes 1000000000 3) ; 1000000007, 1000000009, 1000000021
;; average time 9 ms
(search-for-primes 10000000000 3) ; 10000000019, 10000000033, 10000000061
;; average time 20 ms
(search-for-primes 100000000000 3) ; 100000000003, 100000000019, 100000000057
;; average time 40 ms
(search-for-primes 1000000000000 3); 1000000000039, 1000000000061, 1000000000063
;; average time 115 ms

;; Yes, runs about twice as fast as in 1.22
