
(define cube (λ (x) (* x x x)))

(define (sum-of-cubes p)
  (+ (cube (first p)) (cube (second p))))

(define rama-base-pairs
  (weighted-pairs sum-of-cubes integers integers))

(print-n rama-base-pairs 32)
; (1 1), (1 2), (2 2), (1 3), (2 3), (3 3), (1 4), (2 4), (3 4), (1 5), (4 4), (2 5), (3 5), (4 5), (1 6), (2 6), (3 6), (5 5), (4 6), (5 6), (1 7), (2 7), (3 7), (4 7), (6 6), (5 7), (1 8), (2 8), (3 8), (6 7), (4 8), (5 8), ... 

(define ramanujans
  (stream-filter
   (λ (p) (not (zero? p)))
   (stream-map
    (λ (p1 p2)
       (let ((cubes-p1 (sum-of-cubes p1))
             (cubes-p2 (sum-of-cubes p2)))
         (if (= cubes-p1 cubes-p2)
             cubes-p1
             0)))
    (stream-cdr rama-base-pairs)
    rama-base-pairs)))

(print-n ramanujans 6)
; 1729, 4104, 13832, 20683, 32832, 39312, ... 

