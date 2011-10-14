
;; Definitions of div-poly, div-terms and division dispatcher for polynomials 
;; are added to the file generic-arithmetic/polynomials.scm.

;; Tests
(mul '(polynomial z (3 1)) '(polynomial z (2 1) (0 -1))) 
; '(polynomial z (5 1) (3 -1))

(div '(polynomial z (5 1) (0 -1))
     '(polynomial z (2 1) (0 -1)))
; '((polynomial z (3 1) (1 1)) (polynomial z (1 1) (0 -1)))

(div '(polynomial z)
     '(polynomial z (2 1) (0 -1)))
; '((polynomial z) (polynomial z))

(div '(polynomial z (2 1) (0 -1))
     '(polynomial z (5 1) (3 -1))) 
; '((polynomial z) (polynomial z (2 1) (0 -1)))

(sub '(polynomial z (5 1) (0 -1))
     (mul '(polynomial z (3 1)) 
	  '(polynomial z (2 1) (0 -1)))) 
; '(polynomial z (3 1) (0 -1))

(sort '((2 1) (5 -1) (3 4) (4 -5)) #:key car >) ; '((5 -1) (4 -5) (3 4) (2 1))
(sort '() #:key car >) ; '()

(div '(polynomial x (3 1) (2 -12) (0 -42))
     '(polynomial x (1 1) (0 -3)))
; '((polynomial x (2 1) (1 -9) (0 -27)) (polynomial x (0 -123)))

(div '(polynomial x (3 3) (2 -2) (1 4) (0 -3))
     '(polynomial x (2 1) (1 3) (0 3)))
; '((polynomial x (1 3) (0 -11)) (polynomial x (1 28) (0 30)))

;; ---------------------------------------------------------------------------

;; We should make the polynomials more readable. Let's write some
;; format conversion procedures.

;; Converts polynomial string looking like this: "4x^3 + x^2 - 5"
;; to internal representation: '(polynomial x (3 4) (2 1) (0 -5))
(define (mathpoly->listpoly variable polynomial)
  (define (tag p) (attach-tag 'polynomial p))
  (define (sort-termlist L) (sort L #:key car >))
  (define (mathpoly->termlist)
    (let ((poly-pattern (string-append "[\\+\\-]?\\s*\\d*/?\\d*"
				       (symbol->string variable)
				       "(\\^\\d+)*|[\\+\\-]?\\s*\\d+/?\\d*"))
	  (term-pattern (string-append "([\\+\\-]?)\\s*(\\d*/?\\d*)("
				       (symbol->string variable)
				       "?)\\^?(\\d+)*")))
      (map (λ (termparts) 
	      (let ((coeff (if (equal? (third termparts) "")
			       (cond ((equal? (second termparts) "-") -1)
				     (else 1))
			       (string->number (string-append 
						(second termparts)
						(third  termparts)))))
		    (order (cond ((equal? (fourth termparts) "") 0)
				 ((false? (fifth termparts)) 1)
				 (else (string->number (fifth termparts))))))
		(list order coeff)))
	   (map (λ (term) (regexp-match (pregexp term-pattern) term))
		(regexp-match* (pregexp poly-pattern) polynomial)))))
  (tag (cons variable (sort-termlist (mathpoly->termlist)))))

;; Examples:
(mathpoly->listpoly 'x "12x^3 + x^2 - 5")
; '(polynomial x (3 12) (2 1) (0 -5))

(mathpoly->listpoly 's "s^5 - 16s^2 + 9s - 33")
; '(polynomial s (5 1) (2 -16) (1 9) (0 -33))


;; Converts polynomial looking like this: '(polynomial x (3 4) (2 1) (0 -5))
;; to conventional algebraic form: "4x³ + x² - 5"
(define (listpoly->mathpoly p)
  (define (sort-termlist L) (sort L #:key car >))
  (define (contents datum)
    (if (pair? datum)
	(cdr datum)
	(if (number? datum)
	    datum
	    (error "Bad tagged datum - CONTENTS" datum))))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (let* ((content (contents p))
	 (var (symbol->string (variable content)))
	 (termlist (sort-termlist (term-list content))))
    (let ((stringlist
	   (map (λ (term) 
		   (let ((coeff-string (if (eq? (abs (coeff term)) 1)
					    (if (zero? (order term)) "1" "")
					    (number->string
					     (abs (coeff term)))))
			 (sign-string (if (negative? (coeff term))
					  " - " 
					  " + "))
			 (var-string (if (zero? (order term)) "" var))
			 (power-string (if (or (eq? (order term) 0)
					       (eq? (order term) 1))
					   ""
					   (number->superscript
					    (order term)))))
		     (list sign-string coeff-string var-string power-string)))
		termlist)))
      (if (empty? stringlist)
	  "0"
	  (let ((without-leading-plus
		 (cons (if (equal? (caar stringlist) " + ")
			   (cdar stringlist)
			   (cons "-" (cdar stringlist)))
		       (cdr stringlist))))
	    (apply string-append (apply append without-leading-plus)))))))

;; Makes a superscript string out of the given number
(define (number->superscript number)
  (define uninums #hash((#\1 . "¹")
			(#\2 . "²")
			(#\3 . "³")
			(#\4 . "⁴")
			(#\5 . "⁵")
			(#\6 . "⁶")
			(#\7 . "⁷")
			(#\8 . "⁸")
			(#\9 . "⁹")
			(#\0 . "⁰")))
  (apply string-append
	 (map (λ (digit) (hash-ref uninums digit))
	      (string->list (number->string number)))))

;; Examples:
(number->superscript 450) ; "⁴⁵⁰"

(listpoly->mathpoly '(polynomial x (3 -2) (2 -6) (1 4) (0 5)))
; "-2x³ - 6x² + 4x + 5"

(listpoly->mathpoly poly2)
; "5y⁴ - 2y³ + 4y + 2"

(listpoly->mathpoly '(polynomial z (6 4) (4 -8) (2 -24)))
; "4z⁶ - 8z⁴ - 24z²"


;; Divides polynomial p1 by p2. Both should be lists consisting of variable 
;; as a symbol and algebraic notation as a string, like '(x "2x^4 + 5x^3 - 1").
;; Returns a list containing quotient and remainder in algebraic notation.
(define (divide p1 p2)
  (let ((p1-internal (apply mathpoly->listpoly p1))
	(p2-internal (apply mathpoly->listpoly p2)))
    (let ((result (div p1-internal p2-internal)))
      (list (listpoly->mathpoly (car result))
	    (listpoly->mathpoly (cadr result))))))

;; Tests
(divide '(x "x^3") '(x "x")) ; '("x²" "0")
(divide '(n "4n^5 - 2n^3 + 1") '(n "2n^2 + 2")) ; '("2n³ - 3n" "6n + 1")

(divide '(w "w^3 - 12w^2 -42") '(w "w - 3")) ; '("w² - 9w - 27" "-123")
(divide '(y "3y^3 - 2y^2 + 4y -3") '(y "y^2 + 3y + 3"))
; '("3y - 11" "28y + 30")
(divide '(s "s^3 - 1") '(s "s + 2")) ; '("s² - 2s + 4" "-9")
(divide '(x "x^4 + 4") '(x "x^2 - 5")) ; '("x² + 5" "29")
(divide '(z "z^3 - 3z^2 + 5z - 3") '(z "z - 1")) ; '("z² - 2z + 3" "0")
(divide '(p "p^3 - 4") '(p "2p + 5")) ; '("1/2p² - 5/4p + 25/8" "-157/8")
(divide '(k "2k^3 - 9k^2 + 15") '(k "2k - 5")) ; '("k² - 2k - 5" "-10")
(divide '(r "4r^4 + 3r^3 +2r +1") '(r "r^2 + r +2"))
; '("4r² - r - 7" "11r + 15")

;; And the book example:
(divide '(x "x^5 - 1") '(x "x^2 - 1")) ; '("x³ + x" "x - 1")

;; We define similar procedures for adding, subtracting and multiplying 
;; by first factoring out a shared skeleton:
(define (algebraic-op op p1 p2)
  (let ((p1-internal (apply mathpoly->listpoly p1))
	(p2-internal (apply mathpoly->listpoly p2)))
    (listpoly->mathpoly (op p1-internal p2-internal))))

;; and then making particular operations:
(define (addit p1 p2)
  (algebraic-op add p1 p2))

(define (subit p1 p2)
  (algebraic-op sub p1 p2))

(define (multiply p1 p2)
  (algebraic-op mul p1 p2))

;; Tests
(multiply '(x "x + 1") '(x "x - 1")) ; "x² - 1"
(multiply '(x "x + 1") '(x "x + 1")) ; "x² + 2x + 1"
(multiply '(x "2x + 3") '(x "x^3 - 2x")) ; "2x⁴ + 3x³ - 4x² - 6x"

(addit '(x "2x + 3") '(x "x^3 - 2x")) ; "x³ + 3"
(addit '(x "x^4 -5x^3 +7") '(x "2x^3 + 4x -6")) ; "x⁴ - 3x³ + 4x + 1"

(subit '(x "x^5 -5x^3 +7") '(x "-x^5 - 9x^3 -5")) ; "2x⁵ + 4x³ + 12"
(subit '(x "6x + 2") '(x "6x + 3")) ; "-1"
