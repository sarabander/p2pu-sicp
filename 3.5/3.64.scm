
(define (stream-limit s Δ)
  (let ((first  (stream-ref s 0))
        (second (stream-ref s 1)))
    (if (< (abs (- first second)) Δ)
        second
        (stream-limit (stream-cdr s) Δ))))

(define (new-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(new-sqrt 1 0.00000000001)
(new-sqrt 2 0.00000000001)
(new-sqrt 10 0.00000000001)
(new-sqrt 121 0.00000000001)
(new-sqrt 729 0.00000000001)

