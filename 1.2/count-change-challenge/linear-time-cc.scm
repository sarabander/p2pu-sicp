
;; =========================================================================
;;  This is a solution to the count-change challenge given in the SICP
;;  paragraph just before exercise 1.11. (See also exercise 1.14.)
;;
;;  Growth of the number of steps is reduced from O(n⁵) to O(n).
;;
;;  SPOILER ALERT: If you want to tackle this challenge yourself, 
;;  don't read any further. Or you could try to make this algorithm still
;;  faster by developing the formula further---like making the expression 
;;  of cc(n, 5) closed-form (without using iterative sum). That would 
;;  make the problem solvable in constant time (with reservations 
;;  concerning bignums).
;;
;;  There is a O(1) solution out there. It is given by George Pólya in 1956,
;;  and it uses generating functions to develop a combinatorial formula.
;;  Understanding his solution is another challenge. 
;;
;;  Here is the article: Pólya, G. On Picture-Writing.
;;  The American Mathematical Monthly, Vol. 63, No. 10, Dec., 1956.
;; =========================================================================

(define counter 0) ; for counting procedure calls


;; Fast iterative version of cc {next 3 procedures}
;; ------------------------------------------------

;; Uses the derived formula:
;;   cc(n, 3) = ⌊n/10⌋⌊n/5⌋ - ⌊n/10⌋² + ⌊n/5⌋ + 1   {see further down}
;; to construct a term in the sum 
;;   cc(n, 4) = 
;;   cc(n, 3) + cc(n - 1·25, 3) + cc(n - 2·25, 3) + ... + cc(n - k·25, 3),
;; where cc(n, m) is count-change of amount n using m kinds of coins.
(define (summand n) 
  (λ (k)
     (let ((n/10 (quotient (- n (* k 25)) 10))
	   (n/5  (quotient (- n (* k 25)) 5)))
       (set! counter (+ counter 1))
       (+ (* n/10 n/5)
	  (- (sqr n/10))
	  n/5
	  1))))

;; Adds two subtrees with root nodes cc(n, 4) and cc(n - 50, 5),
;; producing cc(n, 5). Takes advantage of duplicate nodes in both.
;; Accumulates sum of subtree cc(n, 4) in sum1,
;; partial sums of this subtree are accumulated in sum2, 
;; whenever the k in cc(n - k·25, 4) is even {works because 50¢ = 2·25¢}.
(define (sumtree term a next b) 
  (define (iter a sum1 sum2)
    (if (< a b)
	sum2
	(let ((acc (+ sum1 (term a))))
	  (iter (next a)
		acc
		(if (even? a)
		    (+ sum2 acc)
		    sum2)))))
  (iter a 0 0))

;; Calls sumtree with term (summand n), 
;; which is the function applied to summing index k.
;; Index starts with ⌊n/25⌋ and decrements down to 0.
(define (count-change n)
  (sumtree (summand n) (quotient n 25) sub1 0))

;; End of cc ----------------------------


;; Tests
;; -----

(count-change 11)
(count-change 100)
(count-change 1000)


;; Experimental estimation of big-O in time
;; ----------------------------------------

(define (estimation amount)
  (define (logn n x) ; base n logarithm
    (/ (log x) (log n)))
  (begin
    (set! counter 0)
    (newline)
    (letrec ((ways (count-change amount))
	     (c (logn amount counter)))
      (display ways) (display " ways to make change for ") 
      (display amount) (display " cents") (newline)
      (display "number of calls to cc: ") (display counter) (newline)
      (display "the c in O(n^c): ") (display c) (newline))))


;; More tests
;; ----------

   (time (estimation 1000))
   (time (estimation 200000))
   (time (estimation (expt 10 8)))  ;   $1 million, takes 2.4 sec to calculate
;; (time (estimation (expt 10 9)))  ;  $10 million, takes  24 sec
;; (time (estimation (expt 10 10))) ; $100 million, takes 240 sec = 4 min
;; (time (estimation (expt 10 11))) ;   $1 billion, takes 2 hours


;; Order of growth hypothesis
;; --------------------------

;;  Time complexity: O(n)
;; Space complexity: O(n⁴)

;; In reality, duration of the program steps increases with big numbers. 
;; This can be seen already with calculating ways of change to $1 billion.
;; What should have taken 40 min according to O(n) growth, took 2 hours.

;; What comes to space, the O(n⁴) growth follows from the summand formula.
;; It's highest power is n², but the formula is used a number of times 
;; that is proportional to n. This raises the highest power to n³. 

;; The number of partial sums when k is even, is also proportional to n, 
;; so the calculating formula is in effect 4th degree polynome. Thus O(n⁴).

;; Experimental observation confirms this: whenever input grows 10 times,
;; output grows asymptotically 10⁴ times. It is easily tested like this:

;; (define amount (expt 10 8))
;; (/ (count-change (* 10 amount)) (count-change amount) 1.0)
;; => 9999.982900020708

;; (set! amount (expt 10 9))
;; (/ (count-change (* 10 amount)) (count-change amount) 1.0)
;; => 9999.998290000209

;; The same space complexity O(n⁴) also characterizes the original algorithm 
;; in the book. Not O(n) inferred from tree depth. O(n) is valid only when
;; using floating-point numbers instead of bignums.


;; Sketch of the formula derivation
;; --------------------------------

;; The formula cc(n, 3) = ⌊n/10⌋⌊n/5⌋ - ⌊n/10⌋² + ⌊n/5⌋ + 1 is derived
;; step-by-step by observing the process tree. Thanks to regular patterns 
;; and redundancies in the tree, most of the branches can be cut down. 

;; We start the mutilation in the left periphery by observing that
;; cc(n, 1) = 1 regardless of n. That is obvious because there is only one
;; way to make change to any amount using one kind of coin. The whole
;; subtree with root node cc(n, 1) can therefore be replaced with 1.

;; Going up, we notice that cc(n, 2) branches to cc(n, 1), which is just 1,
;; and to cc(n - 5, 2). The branching continues with the same pattern
;; until there is nothing more to subtract from n. There are exactly ⌊n/5⌋
;; nodes cc(n - k·5, 2) down-right from cc(n, 2), (k = 1..⌊n/5⌋).

;; Every such node has cc(n - k·5, 1) as a child node, which is 1.
;; So cc(n, 2) is the sum of all those ones. ⌊n/5⌋ ones come from the right 
;; sub-branch, and single 1 comes from the left sub-branch, giving ⌊n/5⌋ + 1
;; as the value of cc(n, 2).

;; Finding cc(n, 3) is slightly more complicated. We need to sum the terms 
;; ⌊(n - k·10)/5⌋ + 1, where k = 0..⌊n/10⌋. After regrouping and simplifying,
;; and using the well-known formula for the sum of arithmetic series,
;; we arrive at cc(n, 3) = ⌊n/10⌋⌊n/5⌋ - ⌊n/10⌋² + ⌊n/5⌋ + 1. 
