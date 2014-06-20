
(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream) 
                    last-value) 
                 2)))
    (cons-stream 
     (sign-change-detector avpt last-value)
     (make-zero-crossings 
      (stream-cdr input-stream) avpt))))

; This averages the present value with the previous average,
; not with the previous value of the sense data.

;; Produces the same incorrectly smoothed signal that the above
;; make-zero-crossings feeds to sign change detector 
(define (smooth-signal input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream) 
                    last-value) 
                 2)))
    (cons-stream 
     avpt
     (smooth-signal 
      (stream-cdr input-stream) avpt))))

(print-n (smooth-signal sense-data 0) 12)
; 1/2, 5/4, 1.375, 1.1875, 0.84375, 0.371875, -0.8140625, -1.90703125, -1.953515625, -1.2267578125, -0.51337890625, 1.243310546875, ... 

;; This correctly averages present value with previous value
(print-n (stream-map average sense-data (cons-stream 0 sense-data)) 12)
; 1/2, 3/2, 1.75, 1.25, 0.75, 0.2, -1.05, -5/2, -5/2, -1.25, -0.15, 1.6, ... 

;; The actual sensor signal
(print-n sense-data 13)
; 1, 2, 1.5, 1, 0.5, -0.1, -2, -3, -2, -0.5, 0.2, 3, 4, ... 

(print-n (make-zero-crossings sense-data 0) 12)

;; Fixed version
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) 
                    last-value) 
                 2)))
    (cons-stream 
     (sign-change-detector avpt last-avpt)
     (make-zero-crossings 
      (stream-cdr input-stream)
      (stream-car input-stream)
      avpt))))

(print-n (make-zero-crossings sense-data 0 0) 12)
; 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, ... 

