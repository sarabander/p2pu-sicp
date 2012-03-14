
(define (logical-or x y)
  (if (and (or (= x 0) (= x 1))
	   (or (= y 0) (= y 1)))
      (if (or (= x 1) (= y 1))
	  1
	  0)
      (error "Invalid signals:" x y)))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
