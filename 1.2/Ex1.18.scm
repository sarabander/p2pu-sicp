;; Ex 1.18

;; Uses halve, double and xor from 1.17

(define (mult-iter a x)
  (define (mult-iter-help a x b) ; a and x must be positive
    (cond ((zero? a) b)
	  ((even? a) (mult-iter-help (halve a) (double x) b))
	  ((odd?  a) (mult-iter-help (- a 1) x (+ b x)))))
  (if (or (zero? a) (zero? x))
      0
      ((if (xor (negative? a) (negative? x)) - +)
       (mult-iter-help (abs a) (abs x) 0))))


(mult-iter 12 -8)

(mult-iter -11 21)
