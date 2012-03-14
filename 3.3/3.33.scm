
;; Depends on definitions in ch3.scm, section 3.3.5

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(define (averager a b c)
  (let ((s (make-connector))
        (h (make-connector)))
    (adder a b s)
    (multiplier s h c)
    (constant 1/2 h)
    'ok))

(averager a b c)

(probe "a" a)
(probe "b" b)
(probe "c" c)

(set-value! a 10 'user) ; Probe: a = 10
(set-value! b 14 'user) ; Probe: c = 12, Probe: b = 14
(get-value c) ; 12

(forget-value! a 'user)
(forget-value! b 'user)
(forget-value! c 'user)

(set-value! a 5 'user) ; Probe: a = 5
(set-value! c 9 'user) ; Probe: b = 13, Probe: c = 9
(get-value b) ; 13

(forget-value! a 'user)
(forget-value! b 'user)
(forget-value! c 'user)

(set-value! c 4.7 'user)  ; Probe: c = 4.7
(set-value! b -3.3 'user) ; Probe: a = 12.7, Probe: b = -3.3
