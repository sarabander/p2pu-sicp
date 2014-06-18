
(define (div-series numer denom)
  (cond ((not (zero? (stream-car denom)))
         (let ((normalizer (/ (stream-car denom))))
           (mul-series
            (scale-stream numer normalizer)
            (invert-unit-series (scale-stream denom normalizer)))))
        (else (display "Error: denominator has zero constant term.\n"))))

(define tan-series
  (div-series sin-series
              cos-series))

(series-approx tan-series 0 25)        ; 0
(series-approx tan-series 0.523599 25) ; 0.5773505683917448 ≈ tan(π/6)
(series-approx tan-series 0.785398 25) ; 0.999999660556731  ≈ tan(π/4)
(series-approx tan-series 1.047198 25) ; 1.7320122638930575 ≈ tan(π/3)

(print-n tan-series 12)

;; Just for checking the scaling functionality
;(define tan-series
;  (div-series sin-series
;              (scale-stream cos-series 5)))
;
;(series-approx (scale-stream tan-series 5) 0.785398 25)
;; 0.999999660556731

;; For testing error handling
;(define cot-series
;  (div-series cos-series
;              sin-series))
;; Error: denominator has zero constant term.

