
; Example: using integers as input stream
;
;   1 3  6 10 15 ... = partial-sums
; + 2 3  4  5  6 ... = (stream-cdr integers)
; ------------------
; 1 3 6 10 15 21 ... = partial-sums

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))

(print-n (partial-sums integers) 16)
;=> 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136, ... 

(print-n (partial-sums fibs) 16)
;=> 0, 1, 2, 4, 7, 12, 20, 33, 54, 88, 143, 232, 376, 609, 986, 1596, ... 

(print-n (partial-sums primes) 15)
;=> 2, 5, 10, 17, 28, 41, 58, 77, 100, 129, 160, 197, 238, 281, 328, ... 

