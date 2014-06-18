
; To break the series multiplication down to a correct recursive procedure,
; we need to do a bit of algebra on paper. Let the two series be
; 
; s₁ = a₀ + a₁x + a₂x² + a₃x³ + ... ,
; s₂ = b₀ + b₁x + b₂x² + b₃x³ + ... .
;  
; We want to find the product of these series,
; 
; s₁⋅s₂ = (a₀ + a₁x + a₂x² + a₃x³ + ...)(b₀ + b₁x + b₂x² + b₃x³ + ...).
; 
; We can group the first series to two parts,
; 
; s₁⋅s₂ = [a₀ + (a₁x + a₂x² + a₃x³ + ...)]⋅[b₀ + b₁x + b₂x² + b₃x³ + ...]
; = a₀[b₀ + b₁x + b₂x² + b₃x³ + ...]
; + (a₁x + a₂x² + a₃x³ + ...)[b₀ + b₁x + b₂x² + b₃x³ + ...].
; 
; The first part of the above sum can be broken further down to
; 
; a₀b₀ + a₀[b₁x + b₂x² + b₃x³ + ...].
; 
; Now the complete product expression looks like this: s₁⋅s₂ =
; 
; a₀b₀ + [a₀(b₁x + b₂x² + ...) + (a₁x + a₂x² + ...)(b₀ + b₁x + b₂x² + ...)].
; 
; It is easy to see the heads and tails of the series s₁, s₂, and s₁⋅s₂ now.

(define (mul-series s1 s2)
  (cons-stream
   (* (stream-car s1) (stream-car s2))
   (add-streams
    (scale-stream (stream-cdr s2) (stream-car s1))
    (mul-series (stream-cdr s1) s2))))

; Confirm that sin²x + cos²x = 1
(print-n
 (add-streams (mul-series sin-series sin-series)
              (mul-series cos-series cos-series))
 20)
;=> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ... 

