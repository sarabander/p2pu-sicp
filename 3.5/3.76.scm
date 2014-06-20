
;; Smooths the input stream by averaging over two previous elements
(define (smooth s)
  (stream-map average
              (cons-stream 0 s)
              (cons-stream 0 (cons-stream 0 s))))

(print-n (smooth sense-data) 14)
; 0, 1/2, 3/2, 1.75, 1.25, 0.75, 0.2, -1.05, -5/2, -5/2, -1.25, -0.15, 1.6, 7/2, ... 

(define (make-zero-crossings signal conditioner)
  (let ((conditioned-signal (conditioner signal)))
    (stream-map sign-change-detector 
                conditioned-signal 
                (cons-stream 0 conditioned-signal))))

(print-n (make-zero-crossings sense-data smooth) 14)
; 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, ... 

