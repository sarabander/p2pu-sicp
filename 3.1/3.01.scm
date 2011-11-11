
(define (make-accumulator initial-value)
  (let ((acc initial-value))
    (λ (added)
       (set! acc (+ acc added))
       acc)))

(define A (make-accumulator 5))

(A 10) ; 15
(A 10) ; 25

(A -20) ; 5
(A -10) ; -5
(A -10) ; -15
