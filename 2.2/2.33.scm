
(define nil empty)

;; From the book
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))
;; -------------

(define (my-map p sequence)
  (accumulate (λ (x y) (cons (p x) y)) nil sequence))

(my-map add1 '(1 2 3 4))
(my-map sqrt '(100 121 36 49))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(my-append '(1 2 3) '(4 5 6))
(my-append '(a b (c d)) '(e f))

(define (my-length sequence)
  (accumulate (λ (x y) (add1 y)) 0 sequence))

(my-length '())
(my-length '(5 7 3))
