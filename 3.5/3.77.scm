
(define (integral delayed-integrand initial-value dt)
  (cons-stream 
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral 
          (delay (stream-cdr integrand))
          (+ (* dt (stream-car integrand))
             initial-value)
          dt)))))

(stream-ref (solve (lambda (y) y) 1 0.0001) 10000)
; 2.7181459268252266
(exp 1)
; 2.718281828459045

