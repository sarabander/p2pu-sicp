
(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;; We will use the same parameters as in previous exercise.
(stream-ref
 (solve-2nd (λ (dy y) (+ (* -2 dy) (* -1 y)))
            0.0001
            1
            -1)
 10000)
; 0.36786104643292905
(exp -1)
; 0.36787944117144233

