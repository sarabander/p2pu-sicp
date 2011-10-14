
(define P1 (mathpoly->listpoly 'x "x^2 - 2x + 1"))
(define P2 (mathpoly->listpoly 'x "11x^2 + 7"))
(define P3 (mathpoly->listpoly 'x "13x + 5"))

(define Q1 (mul P1 P2))
(define Q2 (mul P1 P3))

(listpoly->mathpoly Q1) ; "11x⁴ - 22x³ + 18x² - 14x + 7"
(listpoly->mathpoly Q2) ; "13x³ - 21x² + 3x + 5"

(listpoly->mathpoly (greatest-common-divisor Q1 Q2))
; "1458/169x² - 2916/169x + 1458/169"

;; We add printf statements to gcd-terms in polynomial package:

;; (define (gcd-terms a b)
;;   (printf "------------------------------------\n")
;;   (printf "a: ~a\n" (listpoly->mathpoly (tag (cons 'x a))))
;;   (printf "b: ~a\n" (listpoly->mathpoly (tag (cons 'x b))))
;;   (if (empty-termlist? b)
;;       a
;;       (gcd-terms b (remainder-terms a b))))

;; Execution trace of (listpoly->mathpoly (greatest-common-divisor Q1 Q2)):

;; ------------------------------------
;; a: 11x⁴ - 22x³ + 18x² - 14x + 7
;; b: 13x³ - 21x² + 3x + 5
;; ------------------------------------
;; a: 13x³ - 21x² + 3x + 5
;; b: 1458/169x² - 2916/169x + 1458/169
;; ------------------------------------
;; a: 1458/169x² - 2916/169x + 1458/169
;; b: 0
;; "1458/169x² - 2916/169x + 1458/169"

;; Where does the 1458/169 come from? We need to dig deeper: trace
;; div-terms also:

;; ------------------------------------
;; a: 11x⁴ - 22x³ + 18x² - 14x + 7
;; b: 13x³ - 21x² + 3x + 5
;; L1: 11x⁴ - 22x³ + 18x² - 14x + 7
;; L2: 13x³ - 21x² + 3x + 5
;; L1: -55/13x³ + 201/13x² - 237/13x + 7
;; L2: 13x³ - 21x² + 3x + 5
;; L1: 1458/169x² - 2916/169x + 1458/169
;; L2: 13x³ - 21x² + 3x + 5
;; ------------------------------------
;; a: 13x³ - 21x² + 3x + 5
;; b: 1458/169x² - 2916/169x + 1458/169
;; L1: 13x³ - 21x² + 3x + 5
;; L2: 1458/169x² - 2916/169x + 1458/169
;; L1: 5x² - 10x + 5
;; L2: 1458/169x² - 2916/169x + 1458/169
;; ------------------------------------
;; a: 1458/169x² - 2916/169x + 1458/169
;; b: 0
;; "1458/169x² - 2916/169x + 1458/169"

(divide '(x "5x^2 - 10x + 5")
	'(x "1458/169x^2 - 2916/169x + 1458/169")) ; '("845/1458" "0")

(* 845/1458 1458/169) ; 5

(divide '(x "11x^4 - 22x^3 + 18x^2 - 14x + 7")
	'(x "13x^3 - 21x^2 + 3x + 5"))
; '("11/13x - 55/169" "1458/169x² - 2916/169x + 1458/169")

(divide '(x "-55/13x^3 + 201/13x^2 - 237/13x + 7")
	'(x "13x^3 - 21x^2 + 3x + 5"))
; '("-55/169" "1458/169x² - 2916/169x + 1458/169")

;; Yes, division of "11x^4 - 22x^3 + 18x^2 - 14x + 7" 
;; by "13x^3 - 21x^2 + 3x + 5" introduces rational coefficients.

;; The resulting GCD is P1 multiplied by constant factor 1458/169.

;; And the result divides both Q1 and Q2:
(divide '(x "11x^4 - 22x^3 + 18x^2 - 14x + 7")
	'(x "1458/169x^2 - 2916/169x + 1458/169"))
; '("1859/1458x² + 1183/1458" "0")

(divide '(x "13x^3 - 21x^2 + 3x + 5")
	'(x "1458/169x^2 - 2916/169x + 1458/169"))
; '("2197/1458x + 845/1458" "0")

;; P1 should also divide both Q1 and Q2:
(divide '(x "11x^4 - 22x^3 + 18x^2 - 14x + 7")
	'(x "x^2 - 2x + 1"))
; '("11x² + 7" "0")

(divide '(x "13x^3 - 21x^2 + 3x + 5")
	'(x "x^2 - 2x + 1"))
; '("13x + 5" "0")

;; Although P1 participates as a factor creating both Q1 and Q2, it is clearly
;; not the greatest common divisor for them, "1458/169x² - 2916/169x + 1458/169"
;; is greater. The reason is that P2 and P3 have GCD bigger than 1:

(listpoly->mathpoly (greatest-common-divisor P2 P3)) ; "1458/169"

;; which is exactly what we expected.
