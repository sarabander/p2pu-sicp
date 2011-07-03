(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;; recursive process 
(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

;; iterative process from Ex 1.16
(define (fast-expt b n)
  (fast-expt-iter 1 b n))

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
	((even? n) (fast-expt-iter a (square b) (/ n 2)))
	((odd?  n) (fast-expt-iter (* a b) b (- n 1)))))

;; In principle, we could use fast-expt, but it makes the calculation 
;; terribly slow. I would guess it's because of the procedure-calling
;; overhead (environment creation) which is magnified by 100000 trials.
;; Better to use inline expressions here.

(search-for-primes 1000 3)
;; average time 7600 ms

;; This hypothesis is supported by the observation that using the
;; iterative fast-expt makes search-for-primes even slower, because
;; of the additional call to fast-expt-iter, which makes bigger
;; environment with 3 parameters.

(search-for-primes 1000 3)
;; average time 8900 ms

;; If we call fast-expt-iter directly, we would expect faster execution:

(define (expmod base exp m)
  (remainder (fast-expt-iter 1 base exp) m))

(search-for-primes 1000 3)
;; average time 8900 ms

;; No, it isn't any faster. This would suggest the overhead comes mainly
;; from environment creation.

;; ...

;; After some pondering, I would disregard much of the above explanation. 
;; The main reason of slowness is that fast-expt produces a huge number,
;; and operations with those are slow. The original expmod keeps the 
;; numbers small by repeatedly taking remainders before squaring or
;; multiplying. Alyssa's expmod calculates the remainder only once.
