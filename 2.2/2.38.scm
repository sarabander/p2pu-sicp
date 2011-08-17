
(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3))  ; 1/6
(fold-right list nil (list 1 2 3)) ; '(1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ; '(((() 1) 2) 3)

;; That property is commutativity (or associativity?)
(fold-right + 0 '(1 2 3 4 5)) ; 15
(fold-left  + 0 '(1 2 3 4 5)) ; 15

(fold-right * 1 '(1 2 3 4 5)) ; 120
(fold-left  * 1 '(1 2 3 4 5)) ; 120

;; More tests to identify the right property

(define identity-matrix1 '((1 0 0) (0 1 0) (0 0 1)))
(define identity-matrix2 '((1 0) (0 1)))

(define matrix1 '((2 3) (-7 1) (5 -2)))
(define matrix2 '((8 -6 3) (-9 4 5)))
(define matrix3 '((4 10) (-1 2) (7 4)))

;; Depends on 2.37
(matrix-*-matrix identity-matrix1 matrix1)
(matrix-*-matrix matrix3 identity-matrix2)

;; Not commutative
(matrix-*-matrix matrix1 matrix2) ; '((-11 0 21) (-65 46 -16) (58 -38 5))
(matrix-*-matrix matrix2 matrix1) ; '((73 12) (-21 -33))

;; Associative
(matrix-*-matrix (matrix-*-matrix matrix1 matrix2) matrix3)
(matrix-*-matrix matrix1 (matrix-*-matrix matrix2 matrix3))
; both '((103 -26) (-418 -622) (305 524))

(fold-left  matrix-*-matrix identity-matrix1 (list matrix1 matrix2 matrix3))
(fold-right matrix-*-matrix identity-matrix2 (list matrix1 matrix2 matrix3))
; both '((103 -26) (-418 -622) (305 524))

;; So the operation should be associative for fold-right and fold-left 
;; to give the same result.
