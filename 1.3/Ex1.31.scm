;; recursive product
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

;; iterative product
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

;; traditional definition of factorial
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; new factorial using product
(define (factorial n)
  (product identity 1 inc n))

(factorial 7)

;; pi/4 formula after John Wallis
(define quarter-pi
  (λ (max-factor)  ; biggest factor in numerator (even)
     (define add2 (λ (x) (+ x 2)))
     (/ (* 2 
	   (product square 4 add2 (- max-factor 2)) 
	   max-factor)
	(product square 3 add2 (- max-factor 1)))))

(define pi-approx
  (λ (max-factor)
     (* 4.0 (quarter-pi max-factor))))

(pi-approx 8))    ; 3.3436734693877552
(pi-approx 100)   ; 3.157339689217565
(pi-approx 10000) ; 3.1417497371492673
