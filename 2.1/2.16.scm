
;; The main reason why algebraically equivalent expressions behave differently
;; in terms of uncertainty propagation is that the current system has "tunnel 
;; vision". It "sees" every arithmetic operation as separate from all others 
;; and performs them pairwise, producing new intervals. These are then again
;; combined step-by-step with other intervals. 

;; The problem with this approach is that whenever there's a repeated 
;; occurrence of a variable in the formula, the program doesn't "remember"
;; having seen it before. It treats this variable independently instead of
;; realising that identically named variables deviate inside their intervals
;; in lockstep, because they are obviously the "same thing". 

;; This explains why the formula with no repeated variables gives a better
;; result with tighter interval. But it's not always possible to transform
;; the formula so that every variable occurs just once. We need a different
;; method that treats the formula as an atomic unit. One possibility is to 
;; find how sensitive the result is to the deviation of each variable,
;; and then add every variable's contribution.

;; "Small quantity" delta r
(define Δr 0.1)

;; Helper functions
(define (nullify lst)
  (map (λ (x) (* x 0)) lst))

(nullify '(6 -3 0 12)) ; '(0 0 0 0)

(define (shift-right lst)
  (take (cons (last lst) lst) (length lst)))

(define (shift-left lst)
  (append (cdr lst) (list (car lst))))

(shift-right '(1 0 0 0)) ; '(0 1 0 0)
(shift-left  '(1 0 0 0)) ; '(0 0 0 1)
(shift-left  '(0 0 0 1)) ; '(0 0 1 0)

((repeated shift-right 3) '(1 0 0 0)) ; '(0 0 0 1)

;; Combines arglist with Δr. Returns list of lists: first list is arglist
;; where Δr is added to first element, second list is arglist where
;; Δr is added to the second element, etc.
(define (delcomb arglist)
  (define startmask (cons Δr (nullify (rest arglist))))
  (define (iter inlist outlist mask)
    (if (empty? inlist)
	outlist
	(iter (rest inlist) 
	      (append outlist (list (map + arglist mask)))
	      (shift-right mask))))
  (iter arglist empty startmask))

;; Delcomb with different implementation
(define (delcomb arglist)
  (define (iter inlist auxlist outlist)
    (if (empty? inlist)
	outlist
	(iter (rest inlist)
	      (append auxlist (list (first inlist)))
	      (append outlist 
		      (list (append auxlist
				    (list (+ (first inlist) Δr)) 
				    (rest inlist)))))))
  (iter arglist empty empty))

;; This takes in quoted function f and list of intervals (arglist). 
;; Arity of f and number of items in arglist must be the same. 
;; Returns an interval whose center is calculated by applying f to the 
;; centers of argument intervals. Uncertainty of the returned value 
;; is found by calculating how much each argument's uncertainty contributes 
;; to the result's uncertainty, and adding these contributions. 
(define (f-interval f arglist)
  (let* ((fargs      (second f))
	 (centers    (map center arglist))
	 (widths     (map width arglist))
	 (centers+Δr (delcomb centers)))
    (if (not (= (length fargs) (length arglist)))
	(printf "Arity of the function and arglist length must match.\n")
	(let* ((f-center (eval-f f centers))
	       (f-width  (apply + (map (λ (center+Δr width) 
					  (* (/ (- (eval-f f center+Δr)
						   f-center)
						Δr)
					     width))
				       centers+Δr widths))))
	  (make-center-width f-center f-width)))))

;; Treats interval widths as standard deviations and adds them in quadrature
(define (f-interval f arglist)
  (let* ((fargs (second f))
	 (centers (map center arglist))
	 (widths (map width arglist))
	 (centers+Δr (delcomb centers)))
    (if (not (= (length fargs) (length arglist)))
	(printf "Arity of the function and arglist length must match.\n")
	(let* ((f-center (eval-f f centers))
	       (f-width  (sqrt 
			  (apply + (map (λ (center+Δr width) 
					   (sqr (* (/ (- (eval-f f center+Δr)
							 f-center)
						      Δr)
						   width)))
					centers+Δr widths)))))
	  (make-center-width f-center f-width)))))

(define (eval-f f arglist)
  (eval (append (list f) arglist)))

;; Parallel resistance formula
(define par3 '(λ (R1 R2) (/ (* R1 R2) (+ R1 R2))))

;; Equivalent to previous formula
(define par4 '(λ (R1 R2) (/ (+ (/ R1) (/ R2)))))

;; Three parallel resistances
(define par5 '(λ (R1 R2 R3) (/ (+ (/ R1) (/ R2) (/ R3)))))

;; We take two resistors...
(define res3 (make-center-percent 39 0.05))
(define res4 (make-center-percent 56 0.05))

;; ...and connect them in parallel...
(center  (f-interval par3 (list res3 res4))) ; 22.989
(percent (f-interval par3 (list res3 res4))) ; 0.0499

;; ...and again using equivalent formula (should give same result)
(center  (f-interval par4 (list res3 res4))) ; 22.989
(percent (f-interval par4 (list res3 res4))) ; 0.0499 (same indeed)

;; Should be third of res3
(center  (f-interval par5 (list res3 res3 res3))) ; 13.0
(percent (f-interval par5 (list res3 res3 res3))) ; 0.0499 
