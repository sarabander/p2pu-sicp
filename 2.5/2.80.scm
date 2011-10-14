
;; The zero package is added to generic-arithmetic/setup.scm.

(define (=zero? x) (apply-generic '=zero? x))

(define (install-zero-package)
  (define numer car)
  (put '=zero? '(scheme-number)
       zero?)
  (put '=zero? '(rational)
       (λ (r) (zero? (numer r))))
  (put '=zero? '(complex)
       (λ (z) (zero? (magnitude z))))
  'done)

(install-zero-package)

;; Tests
(=zero? (make-scheme-number 0))  ; true
(=zero? (make-scheme-number 10)) ; false

(=zero? r1) ; false
(=zero? (make-rational 0 15)) ; true
(=zero? (make-rational 5 3))  ; false

(=zero? c1) ; false
(=zero? c2) ; false
(=zero? (make-complex-from-mag-ang 0 30))   ; true
(=zero? (make-complex-from-mag-ang 10 0))   ; false
(=zero? (make-complex-from-real-imag 0 0))  ; true
(=zero? (make-complex-from-real-imag -7 0)) ; false
(=zero? (make-complex-from-real-imag 0 42)) ; false
