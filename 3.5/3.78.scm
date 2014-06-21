
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

;; Let's choose constants and initial values so
;; that the solution will be y = exp(-t).
(stream-ref (solve-2nd -2 -1 0.0001 1 -1) 10000)
; 0.36786104643292905
(exp -1)
; 0.36787944117144233

