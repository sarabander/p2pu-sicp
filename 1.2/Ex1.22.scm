;; Needs the procedures of 1.21

;; Racket needs this
(define (runtime) (current-milliseconds))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

;; The procedures differ from book originals
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      false))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

(timed-prime-test 242)
(time (timed-prime-test 4398042316799))
(time (timed-prime-test 1125899839733759))

(define (search-for-primes starting-with how-many)
  (cond ((zero? how-many) (void))
	((odd? starting-with)
	 (if (timed-prime-test starting-with)
	     (search-for-primes (+ 2 starting-with) (- how-many 1))
	     (search-for-primes (+ 2 starting-with) how-many)))
	(else (search-for-primes (+ 1 starting-with) how-many))))

(search-for-primes 1000 3) ; 1009, 1013, 1019
(search-for-primes 10000 3) ; 10007, 10009, 10037
(search-for-primes 100000 3) ; 100003, 100019, 100043
(search-for-primes 1000000 3) ; 1000003, 1000033, 1000037

;; search finishes faster than the runtime granularity (1 ms)
;; need to use larger numbers:

(search-for-primes 1000000000 3) ; 1000000007, 1000000009, 1000000021
;; average time 16 ms
(search-for-primes 10000000000 3) ; 10000000019, 10000000033, 10000000061
;; average time 42 ms
(search-for-primes 100000000000 3) ; 100000000003, 100000000019, 100000000057
;; average time 85 ms
(search-for-primes 1000000000000 3); 1000000000039, 1000000000061, 1000000000063
;; average time 200 ms

(* 30 (sqrt 10))

;; Little less than (sqrt 10) growth in time when input grows 10 times.
;; Seems to support the proportionality of number of steps and time.
