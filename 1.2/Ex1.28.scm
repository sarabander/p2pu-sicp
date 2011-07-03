;; Depends on 1.24

;; How many times to run prime-test
(define tries 1000) 

;; Probabilistic test
(define (miller-rabin-prime? n)
  (define (fast-prime n times)
    (cond ((= times 0) true)
	  ((prime-test n) (fast-prime n (- times 1)))
	  (else false)))
  (fast-prime n tries))

(define (prime-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

;; ;; Exhaustive test
;; (define (miller-rabin-prime? n)
;;   (define (fast-prime n a)
;;     (cond ((= a 0) true)
;; 	  ((prime-test n a) (fast-prime n (- a 1)))
;; 	  (else false)))
;;   (fast-prime n (- n 1)))

;; (define (prime-test n a)
;;   (define (try-it a)
;;     (= (expmod a (- n 1) n) 1))
;;   (try-it a))

;; Modified to signal the discovery of nontrivial square root of 1
;; by returning 0
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (let* ((aexp (expmod base (/ exp 2) m))
		(rem  (remainder (square aexp)
				 m)))
	   (if (= rem 1) 
	       (if (or (= aexp 1) (= aexp (- m 1)))
		   1
		   0) ; nontrivial sqrt of 1 discovered!
	       rem)))
	 (else (remainder (* base (expmod base (- exp 1) m))
			  m))))

;; Tests
;; -----

;; Carmichael numbers
(map miller-rabin-prime? '(561 1105 1729 2465 2821 6601 8911 10585 15841)) 
; all false

;; Primes from Wikipedia:

;; Centered decagonal primes
(map miller-rabin-prime? 
     '(11 31 61 101 151 211 281 661 911 1051 1201 1361 1531)) ; all true

;; Dihedral primes
(map miller-rabin-prime?
     '(2 5 11 101 181 1181 1811 18181 108881 110881 118081 120121 121021))
; all true

;; Fibonacci primes
(map miller-rabin-prime?
     '(2 3 5 13 89 233 1597 28657 514229)) ; all true

;; Mersenne primes
(map miller-rabin-prime?
     '(3 7 31 127 8191 131071 524287)) ; all true

;; Primes # 301-320
(map miller-rabin-prime?
     '(1993 1997 1999 2003 2011 2017 2027 2029 2039 2053 
	    2063 2069 2081 2083 2087 2089 2099 2111 2129)) ; all true

;; Non-primes
(map miller-rabin-prime? '(6 9 46 78 27 93)) ; all false
