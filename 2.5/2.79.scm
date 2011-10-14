
;; The equality package is added to generic-arithmetic/setup.scm.

(define (equ? x y) (apply-generic 'equ? x y))

;; First version
(define (install-equality-package)
  (define numer car)
  (define denom cdr)
  (define real-part car)
  (define imag-part cdr)
  (define magnitude car)
  (define angle cdr)
  (put 'equ? '(scheme-number scheme-number) equal?)
  (put 'equ? '(integer integer) equal?)
  (put 'equ? '(real real) equal?)
  (put 'equ? '(rational rational)
       (λ (x y) (and (equal? (numer x) (numer y))
		     (equal? (denom x) (denom y)))))
  (put 'equ? '(complex complex)
       (λ (x y) (equ? x y)))
  (put 'equ? '(rectangular rectangular)
       (λ (x y) (and (equal? (real-part x) (real-part y))
		     (equal? (imag-part x) (imag-part y)))))
  (put 'equ? '(polar polar)
       (λ (x y) (and (equal? (magnitude x) (magnitude y))
		     (equal? (angle x) (angle y)))))
  'done)

;; This doesn't work right because the standard equal? finds 0 and 0.0
;; not equal:
(equal? 0 0.0) ; false
(equal? 0 0)   ; true

;; Second version subtracts the quantities and tests the result for zero:
(define (install-equality-package)
  (define numer car)
  (define denom cdr)
  (define real-part car)
  (define imag-part cdr)
  (define magnitude car)
  (define angle cdr)
  (define myequal? (λ (x y) (zero? (- x y))))
  (put 'equ? '(scheme-number scheme-number) myequal?)
  (put 'equ? '(integer integer) myequal?)
  (put 'equ? '(real real) myequal?)
  (put 'equ? '(rational rational)
       (λ (x y) (and (zero? (- (numer x) (numer y)))
		     (zero? (- (denom x) (denom y))))))
  (put 'equ? '(complex complex)
       (λ (x y) (equ? x y)))
  (put 'equ? '(rectangular rectangular)
       (λ (x y) (and (zero? (- (real-part x) (real-part y)))
		     (zero? (- (imag-part x) (imag-part y))))))
  (put 'equ? '(polar polar)
       (λ (x y) (and (zero? (- (magnitude x) (magnitude y)))
		     (zero? (- (angle x) (angle y))))))
  'done)

(install-equality-package)

;; Tests
(equ? num1 num2) ; false
(equ? num1 num1) ; true

(equ? r1 r2) ; false
(equ? r2 r2) ; true

(equ? c1 c1) ; true
(equ? c2 c2) ; true
(equ? c1 (make-complex-from-real-imag -2 -4)) ; false
