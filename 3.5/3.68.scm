
(define (pairs s t)
  (interleave
   (stream-map
    (lambda (x) 
      (list (stream-car s) x))
    t)
   (pairs (stream-cdr s)
          (stream-cdr t))))

(print-n (pairs integers integers) 3)

; It doesn't work this way, it enters infinite loop.

