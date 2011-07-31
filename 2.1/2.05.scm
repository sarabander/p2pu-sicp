
;; Extracts the exponent of given base from product 2ᵅ3ᵇ held in pack
;; by counting how many times can pack be divided by base without remainder
(define (extract pack base count)
  (if (> (remainder pack base) 0)
      count
      (extract (quotient pack base) 
	       base
	       (add1 count))))

(define (mycons a b)
  (* (expt 2 a) (expt 3 b)))

(define (mycar x)
  (extract x 2 0))

(define (mycdr x)
  (extract x 3 0))

;; Tests
(mycar (mycons 27 13)) ; => 27
(mycdr (mycons 27 13)) ; => 13

(mycdr (mycons 70 0))  ; => 0
