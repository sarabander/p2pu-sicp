
(define (sign-change-detector present previous)
  (cond ((eq? (negative? present) (negative? previous)) 0)
        ((negative? previous) 1)
        (else -1)))

;; Alyssa's initial version
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector 
    (stream-car input-stream) 
    last-value)
   (make-zero-crossings 
    (stream-cdr input-stream)
    (stream-car input-stream))))

;; Helper function to turn a list into a stream
(define (stream-from-list lst)
  (if (null? lst) 
      the-empty-stream
      (cons-stream (car lst)
                   (stream-from-list (cdr lst)))))

(define sense-data
  (stream-from-list
   (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(print-n sense-data 13)
; 1, 2, 1.5, 1, 0.5, -0.1, -2, -3, -2, -0.5, 0.2, 3, 4, ... 

(define zero-crossings 
  (make-zero-crossings sense-data 0))

;; Eva's more compact version
(define zero-crossings
  (stream-map sign-change-detector 
              sense-data 
              (cons-stream 0 sense-data)))

(print-n zero-crossings 13)
; 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, ... 

