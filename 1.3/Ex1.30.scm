;; iterative sum
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

(define (identity x) x)

(define (inc x) (+ x 1))

(sum identity 1 inc 10) ; 55
(sum cube 1 inc 10)     ; 3025

(* 8 (pi-sum 1 1000))      ; 3.139592655589782
(* 8 (pi-sum 1 100000000)) ; 3.1415926335405047
