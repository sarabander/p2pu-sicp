
(define (RC R C dt)
  (λ (currents v0)
     (add-streams
      (scale-stream currents R)
      (integral (scale-stream currents (/ C)) v0 dt))))

(define RC1 (RC 5 1 0.5))

(define step-signal (cons-stream 0 (cons-stream 0 ones)))
(print-n step-signal 20)
; 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ... 

(define step-response (RC1 step-signal 0))
(print-n step-response 15)
; 0, 0, 5, 5.5, 6.0, 6.5, 7.0, 7.5, 8.0, 8.5, 9.0, 9.5, 10.0, 10.5, 11.0, ... 

(define rectangular-pulse
  (sub-streams
   step-signal
   (cons-stream 0 (cons-stream 0 step-signal))))
(print-n rectangular-pulse 10)
; 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, ... 

(print-n (RC1 rectangular-pulse 1) 10)
; 1, 1, 6, 6.5, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, ... 

