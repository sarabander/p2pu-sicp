
;; Let our polynomial be structured as:
;;
;; '(polynomial  <variable>  <term_1>  <term_2> .. <term_n>)
;;
;; where <variable> is the indeterminate, like 'x or 'y,
;; <term_1>  <term_2> .. <term_n>  are terms: 
;;    coeff[n] coeff[n-1] .. coeff[0] when dense,
;;    (order coeff) (order coeff) ..  when sparse.

;; Example polynomials:
(define poly1 '(polynomial x 5 -2 0 4 2))
(define poly2 '(polynomial y (4 5) (3 -2) (1 4) (0 2)))
(define poly3 '(polynomial y))
(define poly4 '(polynomial x 0 0 0))

(define (list-of-zeros? lst)
  (eval (cons 'and 
	      (map (λ (elem)
		      (and (number? elem) 
			   (zero? elem)))
		   lst))))

;; '=zero?' is installed to polynomial package in the file
;; generic-arithmetic/polynomials.scm:
(put '=zero? '(polynomial)
       (λ (p) (list-of-zeros? (contents p))))

;; Tests
(list-of-zeros? '()) ; true
(list-of-zeros? '(3 -3 0)) ; false
(list-of-zeros? '(a b c)) ; false
(list-of-zeros? '(0 0 0)) ; true

(=zero? poly1) ; false
(=zero? poly2) ; false
(=zero? poly3) ; true
(=zero? poly4) ; true

;; Addition and multiplication tests

(add poly3 poly3) ; '(polynomial y)
(add poly2 poly2) ; '(polynomial y (4 10) (3 -4) (1 8) (0 4))
(add poly4 poly4) ; '(polynomial x 0 0 0)
(add poly1 poly4) ; '(polynomial x 5 -2 0 4 2)
(add poly1 poly1) ; '(polynomial x 10 -4 0 8 4)

(mul poly1 poly4) ; '(polynomial x)
(mul poly1 poly1) 
; '(polynomial x (8 25) (7 -20) (6 4) (5 40) (4 4) (3 -8) (2 16) (1 16) (0 4))
(mul poly2 poly2)
; '(polynomial y (8 25) (7 -20) (6 4) (5 40) (4 4) (3 -8) (2 16) (1 16) (0 4))
