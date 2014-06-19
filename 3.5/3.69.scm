
;; Re-establish the original pairs procedure
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x)
                  (append (list (stream-car s)) x))
                (pairs (stream-cdr t) (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define integer-triples (triples integers integers integers))

(print-n integer-triples 200)

;; Try to find a triple violating i ≤ j ≤ k
(print-n
 (stream-map (λ (triple)
                (let ((i (first triple))
                      (j (second triple))
                      (k (third triple)))
                  (if (<= i j k) 0 triple)))
             integer-triples)
 500)
; 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
; not to be found among first 500 triples.

(define square (λ (x) (* x x)))

(define pythagorean-triples
  (stream-filter (λ (triple)
                    (let ((i (first triple))
                          (j (second triple))
                          (k (third triple)))
                      (= (+ (square i) (square j))
                         (square k))))
                 integer-triples))

(print-n pythagorean-triples 5)
; (3 4 5), (6 8 10), (5 12 13), (9 12 15), (8 15 17), ... 

; (More than 5 results can easily consume all remaining memory.)

