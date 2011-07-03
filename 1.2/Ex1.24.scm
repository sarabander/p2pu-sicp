;; Depends on 1.22

;; How many times to run fermat-test
(define tries 100000)

;; Modified to use fast-prime?
(define (start-prime-test n start-time)
  (if (fast-prime? n tries)
      (report-prime n (- (runtime) start-time))
      false))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; To count how many times expmod divides exponent by two
(define halvings 0)
;; and decrements it
(define decrements 0)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (and (set! halvings (+ halvings 1))
	      (remainder (square (expmod base (/ exp 2) m))
			 m)))
	(else
	 (and (set! decrements (+ decrements 1))
	      (remainder (* base (expmod base (- exp 1) m))
			 m)))))

(search-for-primes 1000 3)
;; average time 540 ms
(search-for-primes 1000000 3)
;; average time 970 ms

;; Ratio of execution times should be:
(/ (log 1000000) (log 1000)) ; 2
;; experiment almost confirms this: ratio of (/ 970 540 1.0) is 1.8,
;; the small difference probably comes from this:
;;
;; We could guess that when exponentiating with successive squaring, 
;; the process is less efficient with small exponents than with large ones.
;; With small exponents the algorithm probably decremets the exponent 
;; relatively more often than it divides by 2. Let's test the hypothesis:

;; when using 1000 and 1000000
(/ halvings decrements 1.0) ; 1.13 and 2.04, respectively

;; indeed, our guess is confirmed!

(set! halvings 0)
(set! decrements 0)
