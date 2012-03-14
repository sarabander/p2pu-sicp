
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv const)
  (let ((z (make-connector)))
    (constant const z)
    z))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 0 'user) 
; Probe: Celsius temp = 0
; Probe: Fahrenheit temp = 32

(forget-value! C 'user)
(set-value! F 100 'user)
; Probe: Fahrenheit temp = 100
; Probe: Celsius temp = 340/9
(/ 340 9.) ; 37.78

(forget-value! F 'user)
(set-value! F 98.6 'user)
; Probe: Fahrenheit temp = 98.6
; Probe: Celsius temp = 37
