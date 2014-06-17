
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) 
           den 
           radix)))

(print-n (expand 1 7 10) 20)
;=> 1, 4, 2, 8, 5, 7, 1, 4, 2, 8, 5, 7, 1, 4, 2, 8, 5, 7, 1, 4, ... 

(print-n (expand 3 8 10) 20)
;=> 3, 7, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ... 

; The given procedure computes the decimal expansion of a fraction
; given by 'num(erator)' and 'den(ominator)' in base 'radix'.

; For example, 1/7 expands to recurring decimal 0.(142857)..,
; and 3/8 expands to exact decimal 0.375.

(print-n (expand 22 7 10) 20)
;=> 31, 4, 2, 8, 5, 7, 1, 4, 2, 8, 5, 7, 1, 4, 2, 8, 5, 7, 1, 4, ... 

; Improper fractions expand to a stream whose first number is equal
; or greater than 10. This number should be divided by 10 to get
; the whole number part: 31, 4, 2, 8, ... is 3.1428...

