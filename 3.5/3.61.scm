
(define (invert-unit-series s)
  (cons-stream
   1 (mul-series (scale-stream (stream-cdr s) -1)
                 (invert-unit-series s))))

(print-n
 (mul-series exp-series
             (invert-unit-series exp-series))
 12)
;=> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ... 

(print-n
 (mul-series cos-series
             (invert-unit-series cos-series))
 12)
;=> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ... 

