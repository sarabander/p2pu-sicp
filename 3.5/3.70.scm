
;; Takes two streams of pairs and a weight function
(define (merge-weighted weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((<= (weight s1car) (weight s2car))
                  (cons-stream 
                   s1car 
                   (merge-weighted weight (stream-cdr s1) s2)))
                 (else
                  (cons-stream 
                   s2car 
                   (merge-weighted weight s1 (stream-cdr s2)))))))))

(define (weighted-pairs weight s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    weight
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs weight (stream-cdr s) (stream-cdr t)))))

; 1.
(print-n (weighted-pairs (λ (p) (+ (first p) (second p)))
                         integers
                         integers)
         31)
; (1 1), (1 2), (1 3), (2 2), (1 4), (2 3), (1 5), (2 4), (3 3), (1 6), (2 5), (3 4), (1 7), (2 6), (3 5), (4 4), (1 8), (2 7), (3 6), (4 5), (1 9), (2 8), (3 7), (4 6), (5 5), (1 10), (2 9), (3 8), (4 7), (5 6), (1 11), ...
  
; 2.
(let ((not-divisible-by-2-3-5
       (λ (x) (not (or (divisible? x 2)
                       (divisible? x 3)
                       (divisible? x 5)))))
      (weight-fn
       (λ (p) (let ((i (first p)) (j (second p)))
                (+ (* 2 i) (* 3 j) (* 5 i j))))))
  (let ((our-stream-of-pairs
         (weighted-pairs weight-fn
                         (stream-filter not-divisible-by-2-3-5 integers)
                         (stream-filter not-divisible-by-2-3-5 integers))))
    (print-n our-stream-of-pairs 44)))
; (1 1), (1 7), (1 11), (1 13), (1 17), (1 19), (1 23), (1 29), (1 31), (7 7), (1 37), (1 41), (1 43), (1 47), (1 49), (1 53), (7 11), (1 59), (1 61), (7 13), (1 67), (1 71), (1 73), (1 77), (1 79), (7 17), (11 11), (1 83), (1 89), (1 91), (7 19), (11 13), (1 97), (1 101), (1 103), (1 107), (1 109), (7 23), (1 113), (13 13), (1 119), (1 121), (11 17), (1 127), ... 

