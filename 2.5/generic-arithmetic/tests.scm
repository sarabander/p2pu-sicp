
;; Some numbers to use in tests
(define num1 (make-scheme-number 12))
(define num2 (make-scheme-number 4))

(define int1 (make-integer 7))
(define int2 (make-integer 11))

(define r1 (make-rational 3 4))
(define r2 (make-rational 5 6))
(define r3 (make-rational 8 1))

(define real1 (make-real 2.625))
(define real2 (make-real pi))
(define real3 (make-real 14.0))

(define c1 (make-complex-from-real-imag 3 4))
(define c2 (make-complex-from-mag-ang (sqrt 2) (/ pi 4)))
(define c3 (make-complex-from-real-imag 2.5 0))
;; -------------------------------------------------------

(add
 ((get 'make 'real) 7.1)
 ((get 'make 'real) 4.0)) ; '(real . 11.1)

(add num1 num2) ; '(scheme-number . 16)
(sub num1 num2) ; '(scheme-number . 8)
(mul num1 num2) ; '(scheme-number . 48)
(div num1 num2) ; '(scheme-number . 3)

(add r1 r2) ; '(rational 19 . 12)
(sub r1 r2) ; '(rational -1 . 12)
(mul r1 r2) ; '(rational 5 . 8)
(div r1 r2) ; '(rational 9 . 10)

(apply-generic 'numer r1) ; 3
(numer r1) ; 3
(denom r1) ; 4

(magnitude c1) ; 5
(magnitude c2) ; 1.4142135623730951 = (sqrt 2)
(angle c1)     ; 0.9272952180016122
(angle c2)     ; 0.7853981633974483 = (/ pi 4)

(real-part c1) ; 3
(real-part c2) ; 1.0
(imag-part c1) ; 4
(imag-part c2) ; 1.0

(add c1 c2) ; '(complex rectangular 4.0 . 5.0)
(sub c1 c2) ; '(complex rectangular 2.0 . 3.0)
(mul c1 c2) ; '(complex polar 7.0710678118654755 . 1.7126933813990606)
(div c1 c2) ; '(complex polar 3.5355339059327373 . 0.1418970546041639)

(square 1.5)   ; 2.25
(square num1)  ; 144
(square int1)  ; 49
(square int2)  ; 121
(square r1)    ; 9/16
(square real3) ; 196.0

(sin  1.68) ; 
(sine 1.68) ; 
(sine num1) ; 
(sine int1) ; 
(sine r2)   ; 
(sine real1); 

(cos    1.68) ; 
(cosine 1.68) ; 
(cosine num1) ; 
(cosine int1) ; 
(cosine r2)   ; 
(cosine real1); 

(atan     1.68) ; 
(atangent 1.68) ; 
(atangent num1) ; 
(atangent int1) ; 
(atangent r2)   ; 
(atangent real1); 
;; -------------------------------------------------------

;; Complex numbers with integer, rational and real parts

(define c4 (make-complex-from-real-imag (make-integer 3) (make-integer 8)))
(define c5 (make-complex-from-real-imag (make-rational 5 3) 
					(make-rational 3 7)))

; this breaks 'drop', need to use apply-generic from 2.84.
(add c4 c4) ; '(complex rectangular (integer . 6) integer . 16)
(add c5 c5) ; '(complex rectangular (rational 10 . 3) rational 6 . 7)
(sub c5 c5) 
(mul c5 c4)
(div c4 c5)

(add int1 r3)    ; '(rational 15 . 1)
(add int2 real1) ; '(real . 13.625)
(add 3 r1)       ; '(real . 3.75)

;; Coercion tests
((get-coercion 'scheme-number 'complex) num2) ; '(complex rectangular 4 . 0)
((get-coercion 'scheme-number 'scheme-number) num1) ; '(scheme-number . 12)
((get-coercion 'complex 'complex) c1) ; '(complex rectangular 3 . 4)
((get-coercion 'integer 'rational) num1) ; '(rational 12 . 1)
((get-coercion 'rational 'real) '(rational 12 . 1)) ; '(real 12.0)
((get-coercion 'real 'complex) '(real 12.0)) 
; '(complex rectangular (12.0) . 0)
((get-coercion 'rational 'real) (make-rational 1 8)) ; '(real 0.125)

