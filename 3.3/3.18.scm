
(define (cyclic? lst)
  (let ((pair-count (count-pairs lst)))
    (define (step-through steps lst)
      (cond ((and (zero? steps) (null? lst)) #f)
            ((zero? steps) #t)
            (else (step-through (- steps 1) (cdr lst)))))
    (step-through pair-count lst)))

(cyclic? '()) ; #f
(cyclic? '(m)) ; #f
(cyclic? '(4 0 3 -1)) ; #f
(cyclic? z) ; #t

