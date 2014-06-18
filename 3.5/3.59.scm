
; 1.
(define (integrate-series s)
  (mul-streams s (stream-map / integers)))

; 2.
(define exp-series
  (cons-stream 
   1 (integrate-series exp-series)))

(print-n exp-series 9)
;=> 1, 1, 1/2, 1/6, 1/24, 1/120, 1/720, 1/5040, 1/40320, 1/362880, ... 

; It is the stream of reciprocals of factorials:
(print-n (stream-map / factorials) 9)
;=> 1, 1, 1/2, 1/6, 1/24, 1/120, 1/720, 1/5040, 1/40320, 1/362880, ... 

(define cos-series
  (cons-stream
   1 (scale-stream (integrate-series sin-series) -1)))

(define sin-series
  (cons-stream
   0 (integrate-series cos-series)))

(print-n cos-series 12)
;=> 1, 0, -1/2, 0, 1/24, 0, -1/720, 0, 1/40320, 0, -1/3628800, 0, ... 

(print-n sin-series 12)
;=> 0, 1, 0, -1/6, 0, 1/120, 0, -1/5040, 0, 1/362880, 0, -1/39916800, ... 

;---------------------------------------------------------------------------

; To produce the sequence 1, x, x², x³, x⁴, ...
(define (geometric-sequence x)
  (cons-stream
   1 (scale-stream (geometric-sequence x) x)))

(print-n (geometric-sequence 2) 12)
;=> 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, ... 
(print-n (geometric-sequence 1/2) 11)
;=> 1, 1/2, 1/4, 1/8, 1/16, 1/32, 1/64, 1/128, 1/256, 1/512, 1/1024, ... 

; Approximation of a series at x having n terms
(define (series-approx series x n)
  (exact->inexact
   (stream-ref
    (partial-sums
     (mul-streams series (geometric-sequence x)))
    n)))

(series-approx exp-series 1 25)
; 2.718281828459045
(exp 1)
; 2.718281828459045

(series-approx exp-series 3 25)
; 20.08553692318766
(exp 3)
; 20.08553692318767

(series-approx sin-series 0 25)        ; 0
(series-approx cos-series 0 25)        ; 1
(series-approx sin-series 1.57 25)     ; .9999996829318349 ≈ sin(π/2)
(series-approx cos-series 1.047198 25) ; .4999996113248019 ≈ cos(π/3)
(series-approx sin-series 0.785398 25) ; .7071066656470942 ≈ sin(π/4)
(series-approx cos-series 0.785398 25) ; .7071068967259818 ≈ cos(π/4)
(/ (sqrt 2))                           ; .7071067811865475

