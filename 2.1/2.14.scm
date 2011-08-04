
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

(define one (make-interval 1 1))

;; A couple of resistors with 2% and 5% tolerance
(define res3 (make-center-percent 39 0.02))
(define res4 (make-center-percent 56 0.05))

(center  (par1 res3 res4)) ; 23.11
(percent (par1 res3 res4)) ; 0.107

(center  (par2 res3 res4)) ; 22.98
(percent (par2 res3 res4)) ; 0.032

;; Results are different indeed, Lem is right

;; Experiments with interval arithmetic
(center  (div-interval res3 res3)) ; 1.0008
(percent (div-interval res3 res3)) ; 0.03998

(center  (div-interval res3 res4)) ; 0.6989
(percent (div-interval res3 res4)) ; 0.0699 (dividing adds the uncertainties)

(center  (mul-interval res3 res4)) ; 2186   (should be 2184)
(percent (mul-interval res3 res4)) ; 0.0699 (multiplying adds them too)

(center  (div-interval one res3))  ; 0.0257 (almost right)
(percent (div-interval one res3))  ; 0.02 
(percent (div-interval one res4))  ; 0.05 
;; (taking reciprocal preserves the uncertainty)

(center  (add-interval res3 res4)) ; 95
(percent (add-interval res3 res4)) ; 0.0377
;; (uncertainty in adding is a bit more than the average of the tolerances)

(center  (sub-interval res4 res3)) ; 17
(percent (sub-interval res4 res3)) ; 0.21
;; (uncertainty in subtracting depends heavily on the difference -- 
;; as difference gets smaller, relative uncertainty gets bigger)
