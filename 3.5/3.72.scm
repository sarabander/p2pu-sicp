
(define (sum-of-squares p)
  (+ (square (first p)) (square (second p))))

(define square-base-pairs
  (weighted-pairs sum-of-squares integers integers))

(define threeway-squaresums
  (stream-filter
   (λ (p) (not (null? p)))
   (stream-map
    (λ (p1 p2 p3)
       (let ((sq1 (sum-of-squares p1))
             (sq2 (sum-of-squares p2))
             (sq3 (sum-of-squares p3)))
         (if (= sq1 sq2 sq3)
             (list sq1 p1 p2 p3)
             '())))
    (stream-cdr (stream-cdr square-base-pairs))
    (stream-cdr square-base-pairs)
    square-base-pairs)))

(print-n threeway-squaresums 12)
; (325 (10 15) (6 17) (1 18)),
; (425 (13 16) (8 19) (5 20)),
; (650 (17 19) (11 23) (5 25)),
; (725 (14 23) (10 25) (7 26)),
; (845 (19 22) (13 26) (2 29)),
; (850 (15 25) (11 27) (3 29)),
; (925 (21 22) (14 27) (5 30)),
; (1025 (20 25) (8 31) (1 32)),
; (1105 (12 31) (9 32) (4 33)),
; (1105 (23 24) (12 31) (9 32)),
; (1250 (25 25) (17 31) (5 35)),
; (1300 (20 30) (12 34) (2 36)),
; ... 

