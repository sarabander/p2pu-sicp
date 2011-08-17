
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      empty
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (λ (row) (dot-product row v)) m))

(matrix-*-vector '((2 3) (1 4) (-1 0)) '(3 5)) ; '(21 23 -3)

(define (transpose mat)
  (accumulate-n cons empty mat))

(transpose '((2 3) (1 4) (-1 0))) ; '((2 1 -1) (3 4 0))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (λ (row) (matrix-*-vector cols row)) m)))

(matrix-*-matrix '((1 2) (3 4) (5 6))
		 '((7 9 11) (8 10 12)))
; '((23 29 35) (53 67 81) (83 105 127))
