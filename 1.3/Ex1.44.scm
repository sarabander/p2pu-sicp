;; Depends on 1.43

(define dx 0.001)

(define (smooth f)
  (λ (x)
     (/ (+ (f (- x dx))
	   (f x)
	   (f (+ x dx))) 
	3)))

(sin (/ pi 6)) ; 0.5

((repeated (smooth sin) 8) (/ pi 6))  ; 0.3943356187201898

((repeated (smooth sin) 20) (/ pi 6)) ; 0.3081501740128094

;; which is logical considering the sinusoid's downward curvature at π/6
