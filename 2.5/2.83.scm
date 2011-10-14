
;; integer → rational → real → complex

;; See generic-arithmetic/setup.scm for coercion definitions.

;; Raising functions
(define raise-integer  (get-coercion 'integer  'rational))
(define raise-rational (get-coercion 'rational 'real))
(define raise-real     (get-coercion 'real     'complex))
(define (raise-complex z) z)

;; Generic raise
(define (raise num)
  ((get-coercion (type-tag num) 'raise) num))

(define (install-raise-package)
  (put-coercion 'integer  'raise raise-integer)
  (put-coercion 'rational 'raise raise-rational)
  (put-coercion 'real     'raise raise-real)
  (put-coercion 'complex  'raise raise-complex)
  'done)

(install-raise-package)

;; Tests
(raise-integer num2)         ; '(rational 4 . 1)
(raise-rational r1)          ; '(real 0.75)
(raise-real (make-real 6.2)) ; '(complex rectangular (6.2) . 0)
(raise-complex c1)           ; unchanged

(raise (make-integer 7)) ; '(rational 7 . 1)
(raise r1)               ; '(real . 0.75)
(raise (make-real 5.3))  ; '(complex rectangular 5.3 . 0)
(raise c1)               ; '(complex rectangular 3 . 4)
