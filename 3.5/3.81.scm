
(define random-init 7)

(define generator-stream
  (cons-stream '(generate) generator-stream))

(print-n generator-stream 3)
; (generate), (generate), (generate), ... 
               
(define reset-and-generate
  (cons-stream '(reset 3) generator-stream))

(print-n reset-and-generate 3)
; (reset 3), (generate), (generate), ... 

(define (rand-stream request-stream)
  (cons-stream
   random-init
   (stream-map
    (lambda (rand request)
      (cond ((eq? (first request) 'reset)
             (second request))
            ((eq? (first request) 'generate)
             (rand-update rand))
            (else 0)))
    (rand-stream request-stream)
    request-stream)))

(print-n (rand-stream generator-stream) 10)
; 7, 88, 116, 110, 75, 19, 31, 101, 86, 62, ... 

(print-n (rand-stream reset-and-generate) 10)
; 7, 3, 107, 121, 118, 37, 9, 15, 50, 106, ... 

