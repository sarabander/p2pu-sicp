
;; The golden ratio comes from this: 𝜑 = a/b = (a + b)/a = 1 + b/a = 1 + 1/𝜑.
;; It is already clear from definition that 𝜑 is a fixed point of x ↦ 1 + 1/x.
;; Another way to prove this is to use the exact value of 𝜑 in x ↦ 1 + 1/x.

;; Multiplying both sides of 𝜑 = 1 + 1/𝜑 by 𝜑, we get 𝜑² = 𝜑 + 1.
;; Which numerical values satisfy this equation?
;; Rearranging, we get 𝜑² - 𝜑 - 1 = 0, where 𝜑 = (1 ± √5)/2 ≈ 1.6180... 

;; We will put this value into the right side of x ↦ 1 + 1/x and hope to
;; arrive at the left side: 1 + 1/𝜑 = 1 + 2/(1 ± √5) = (1 ± √5 + 2)/(1 ± √5)
;;   (3 ± √5)(1 ∓ √5)   3 ∓ 3√5 ± √5 - 5   -2 ∓ 2√5   1 ± √5
;; = ---------------- = ---------------- = -------- = ------ = 𝜑. ∎
;;   (1 ± √5)(1 ∓ √5)        1 - 5            -4        2

;; We will now compute the golden ratio using the provided procedure:

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(fixed-point (λ (x) (+ 1 (/ x))) 1.0) ; => 1.6180327868852458
