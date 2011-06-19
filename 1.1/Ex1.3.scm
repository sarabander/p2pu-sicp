;; Exercise 1.3

(define (sq x) (* x x))

(define (sqsum x y) (+ (sq x) (sq y)))

(define (twolargest-sqsum a b c)
  (cond ((and (<= a b) (<= a c)) (sqsum b c))
	((and (<= b a) (<= b c)) (sqsum a c))
	((and (<= c a) (<= c b)) (sqsum a b))))

(define tlss twolargest-sqsum)

;; test:
(tlss 2 3 4)
; 25

(tlss 4 3 8)
; 80

(tlss 7 11 3)
; 170
