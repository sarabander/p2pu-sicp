
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 - SQUARER"
                   (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (square (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(define e (make-connector))
(define f (make-connector))

(squarer e f)

(probe "e" e)
(probe "f" f)

(set-value! e 10 'user) ; Probe: e = 10, Probe: f = 100
;; Right!

(forget-value! e 'user)
(forget-value! f 'user)

(set-value! f 625 'user) ; Probe: f = 625, Probe: e = 25
;; Right!
