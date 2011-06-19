;; Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; (if (> b 0) + -) evaluates to either + or -
; depending on b. If b is positive, we get the combination
; (+ a b), otherwise (- a b) in place of ((if (> b 0) + -) a b)
;
; This is a little shorter, less repetitious way to write the following:
; (define (a-plus-abs-b a b)
;   (if (> b 0) (+ a b) (- a b))) 
;
; Imagine a procedure with more arguments. Depending if a is positive
; or negative, this adds or subtracts all the arguments:
;
; (define (a-plus-or-minus-rest a b c d e f)
;  ((if (> a 0) + -) a b c d e f))
;
; More verbose definition:
;
; (define (a-plus-or-minus-rest a b c d e)
;   (if (> a 0) (+ a b c d e f) (- a b c d e f)))
;
; We avoid typing all the formal parameter names twice. 
; It becomes worse if the names are longer.

;; test:

(a-plus-abs-b 5 -10)
; 15

(a-plus-abs-b -7 -12)
; 5

(a-plus-abs-b 3 36)
; 39
