(define (count-change amount)
  (cc amount 5))

;; Original version of cc with counter added
(define (cc amount kinds-of-coins)
  (set! counter (+ counter 1))
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

;; Second version of cc: 
;; ---------------------
;; condition
;;   ((= amount 0) 1)
;; replaced by
;;   ((or (= amount 0) (= kinds-of-coins 1)) 1);
;; this eliminates all subtrees with root node (cc amount 1)
;; and reduces number of steps significantly
(define (cc amount kinds-of-coins)
  (set! counter (+ counter 1))
  (cond ((or (= amount 0) (= kinds-of-coins 1))  1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

;; Third version: 
;; --------------
;; additional condition
;;   ((= kinds-of-coins 2) (+ 1 (quotient amount 5)))
;; eliminates all subtrees with root node (cc amount 2)
;; and reduces number of steps even further
(define (cc amount kinds-of-coins)
  (set! counter (+ counter 1))
  (cond ((= kinds-of-coins 2) (+ 1 (quotient amount 5)))
	((or (= kinds-of-coins 1) (= amount 0)) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

;; Experimental estimation of big-O in time

;; counts the cc procedure calls
(define counter 0)

;; Base n logarithm
(define (logn n x)
  (/ (log x) (log n)))

(define (estimation amount)
  (begin
    (set! counter 0)
    (newline)
    (letrec ((ways (count-change amount))
	     (c (logn amount counter)))
      (display ways) (display " ways to make change for ") 
      (display amount) (display " cents") (newline)
      (display "number of calls to cc: ") (display counter) (newline)
      (display "the c in O(n^c): ") (display c) (newline))))

;                   # of ways  3rd version  2nd version  original version
(estimation 11)   ; 4          c -> 0.92    c -> 1.18    c -> 1.67
(estimation 100)  ; 292             0.96         1.39         2.10
(estimation 200)  ; 2435            1.14         1.60         2.33
(estimation 500)  ; 59576           1.35         1.88         2.63
(estimation 1000) ; 801451          1.50         2.07         2.84
(estimation 1500) ; 3820626         1.57         2.17
(estimation 2000) ; 11712101        1.62         2.23
(estimation 2500) ; 28070876        1.66         2.28
(estimation 3000) ; 57491951        1.69
(estimation 4000) ; 178901001       1.73
(estimation 6000) ; 891646701       1.79
(estimation 10000); 6794128501      1.86

(time (estimation 12000))

;; Bill the Lizard has a very good explanation of the time complexity here:
;; http://www.billthelizard.com/2009/12/sicp-exercise-114-counting-change.html

;; He shows that the number of steps grows as O(n⁵).

;; I claim that the third version of cc above reduces it to O(n³).
;; 55 steps to count ways of change to 11 cents is reduced to 9 steps!

