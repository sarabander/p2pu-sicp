
;; Depends on some preceding exercises

;; From the book
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))
;; ----------------

(define res3 (make-center-percent 39 0.05))
(define res4 (make-center-percent 56 0.05))

(center  (par1 res3 res4)) ; 23.22
(percent (par1 res3 res4)) ; 0.149 

(center  (par2 res3 res4)) ; 22.99 (center is closer to actual value)
(percent (par2 res3 res4)) ; 0.05  (par2 indeed makes tighter tolerances)

(center  (div-interval res3 res3)) ; 1.005  (should be exactly 1)
(percent (div-interval res3 res3)) ; 0.0998 (should be 0)

(center  (div-interval res3 res4)) ; 0.6999
(percent (div-interval res3 res4)) ; 0.0998 (dividing adds the uncertainties)

;; She is right. We saw in 2.14 that different operations propagate relative
;; tolerances differently - multiplication and division adds them, 
;; addition averages them and reciprocal preserves them. Subtraction usually
;; scales them up.

;; par1 has one multiplication, addition and division. We took the case where
;; both resistances have the same percentage tolerance, 5%. Multiplication
;; in numerator doubles the tolerance to 10%. Addition in denominator 
;; preserves tolerance at 5%. Dividing numerator by denominator adds the
;; corresponding tolerances, producing 15%. Evaluating par1 confirms this:
(percent (par1 res3 res4)) ; 0.149

;; par2 has 1 addition and 3 reciprocals that all preserve the tolerance at 5%:
(percent (par2 res3 res4)) ; 0.05

;; Thus, we should prefer additions and reciprocals to multiplications and 
;; divisions.
