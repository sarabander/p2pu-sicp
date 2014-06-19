
(print-n (pairs integers integers) 300)

;; The following formulas are conjectured from observation of the stream.

;; The index of pair (1, n) in the stream, counting from zero.
;; Same as the number of pairs preceding (1, n).
(define (index-of-1:n n)
  (- (* 2 n) 3))   ; 2n − 3

;; The index of pair (n, n+1) in the stream.
(define (index-of-n:n+1 n)
  (- (average (expt 2 n) (expt 2 (add1 n)))   ; average(2ⁿ, 2ⁿ⁺¹) − 2
     2))

;; The index of pair (n, n) in the stream.
(define (index-of-n:n n)
  (- (expt 2 n) 2))   ; 2ⁿ − 2

(map index-of-n:n+1 (range 1 7))

(stream-ref (pairs integers integers) (index-of-1:n 100)) ; '(1 100)
(stream-ref (pairs integers integers) (index-of-1:n 132)) ; '(1 132)
(stream-ref (pairs integers integers) (index-of-n:n+1 4)) ; '(4 5)
(stream-ref (pairs integers integers) (index-of-n:n 12))  ; '(12 12)

;; The number of pairs preceding the given pair:
;; (1, 100)
(index-of-1:n 100)  ; 197
;; (99, 100)
(index-of-n:n+1 99) ; 950737950171172051122527404030
;; (100, 100)
(index-of-n:n 100)  ; 1267650600228229401496703205374

