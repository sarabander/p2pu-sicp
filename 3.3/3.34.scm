
(define (squarer a b)
  (multiplier a a b))

(define e (make-connector))
(define f (make-connector))

(squarer e f)

(probe "e" e)
(probe "f" f)

(set-value! e 10 'user) ; Probe: e = 10, Probe: f = 100
(get-value f) ; 100
;; Right!

(forget-value! e 'user)
(forget-value! f 'user)

(set-value! f 144 'user) ; Probe: f = 144
(get-value e) ; 10 
;; Wrong! 

;; Previous value of e stays in the input. If this were the first
;; invocation, e would be #f. 

;; The squarer works only in one direction, there's no way to produce
;; square roots by a single division operation.
