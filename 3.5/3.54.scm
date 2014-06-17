
(define (mul-streams s1 s2) 
  (stream-map * s1 s2))

; Check:
(print-n (mul-streams integers integers) 15)
;=> 1, 4, 9, 16, 25, 36, 49, 64, 81, 100, 121, 144, 169, 196, 225, ... 

; Construction of factorials stream
;
;   1 2 3  4   5   6 ... = integers
; * 1 1 2  6  24 120 ... = factorials
; ----------------------
; 1 1 2 6 24 120 720 ... = factorials

(define factorials 
  (cons-stream 1 (mul-streams integers factorials)))

(print-n factorials 12)
;=> 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800, ... 

(stream-ref factorials 7) ;=> 5040

; We will check against the usual recursive definition:
(define (fact n)
  (if (zero? n) 1 (* n (fact (- n 1)))))

(define rec-facts (stream-map fact (cons-stream 0 integers)))

(print-n rec-facts 12)
;=> 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800, ... 

(define (sub-streams s1 s2) 
  (stream-map - s1 s2))

(print-n (sub-streams factorials rec-facts) 20)
;=> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ... 

