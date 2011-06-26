(define (square x) (* x x))

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
	((even? n) (fast-expt-iter a (square b) (/ n 2)))
	((odd?  n) (fast-expt-iter (* a b) b (- n 1)))))

(define (fast-expt2 b n)
  (fast-expt-iter 1 b n))

(fast-expt2 2 15)

;; space: O(1), time: O(log n)
