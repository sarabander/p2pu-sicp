;; Ex 1.15

(define counter 0)

(define (1+ n) (+ n 1))

(define (cube x) (* x x x))

(define (p x) 
  (and (set! counter (1+ counter)) ; for counting invocations of p
       (- (* 3 x) (* 4 (cube x)))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; a) Evaluating (sine 12.15).
;;
;; We need to get sine's argument down to 0.1 by dividing 12.15
;; by 121.5 or greater number. Every time p is applied, argument
;; gets divided by 3. How many times we need to divide 12.15 by 3
;; to get down to 0.1? Let's find the x from 3^x = 121.5:

;; x = log3(121.5) = log(121.5) / log(3):
(define x (/ (log 121.5) (log 3))) ; x = 4.37

;; We need to divide 5 times.

;; check:
(expt 3 x) ; => 121.5

;; So the tree height is 6 when root node is included. 
;; The process p creates a binary tree of size 2^6 - 1 = 63.

;; We can check how many times p is applied:
(sine 12.15)
counter ; => 5
(set! counter 0) ; re-initialize after test!

;; Five times only? Didn't expect that! But wait, we have applicative-order
;; evaluation, not normal-order, so applying p is postponed until
;; it's argument can be evaluated. We get delayed evaluation chain like this:
(p (p (p (p (p 0.05)))))

;; Even if we have normal-order evaluation, the tree is a lot bigger
;; than 63 nodes because of the cubing operation I overlooked the first time.

;; b) (sine a)
;;
;; Because we got number 5 from taking a logarithm of angle a,
;; the order of growth is O(log a) in both space and time.
;;
;; Put another way: if we have a division of the angle by a constant at every
;; invocation of the procedure, the number of steps grows logarithmically.
