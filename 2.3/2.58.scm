
;; The original deriv procedure from the book. Exponentiation case added.
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation (base exp)
					     (sub1 (exponent exp))))
	  (deriv (base exp) var)))
	(else
	 (error "unknown expression type - DERIV" exp))))

;; a.

(define (triplet? x)
  (= (length x) 3))

;; Is the expression in infix binary form?
(define (binary? operation exp)
  (and (triplet? exp) (eq? (cadr exp) operation)))

(define (left-operand exp)
  (car exp))

(define (right-operand exp)
  (caddr exp))

;; sum
(define (sum? x)
  (binary? '+ x))

(define (addend s) (left-operand s))

(define (augend s) (right-operand s))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+ a2))))

;; product
(define (product? x)
  (binary? '* x))

(define (multiplier p) (left-operand p))

(define (multiplicand p) (right-operand p))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))

;; exponentiation
(define (exponentiation? x)
  (binary? '** x))

(define (base e) (left-operand e))

(define (exponent e) (right-operand e))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
	((=number? exponent 1) base)
	((and (number? base) (number? exponent)) (** base exponent))
	(else (list base '** exponent))))

(define ** expt)

;; Tests
(deriv '((3 * (x * y)) + (-5 * y)) 'y) ; '((3 * x) + -5)
(deriv '(x + (3 * (x + (y + 2)))) 'x)  ; 4

;; b.

;; Return the subexpression before or after first occurrence of stated symbol,
;; "part" should be 'before or 'after.
(define (extract part symbol exp)
  (define (iter subexp remaining)
    (if (or (empty? remaining)
	    (eq? (car remaining) symbol)) 
	(cond ((eq? part 'before) subexp)
	      ((eq? part 'after) 
	       (if (empty? remaining) empty (cdr remaining)))
	      (else (error "Unclear, do you mean 'before or 'after?")))
	(iter (append subexp (list (car remaining))) 
	      (cdr remaining))))
  (let ((result (iter empty exp)))
    (if (= (length result) 1) 
	(car result)
	result)))

;; Some unit tests
(extract 'before '+ '(8 * y + 9 * x)) ; '(8 * y)
(extract 'after  '+ '(8 * y + 9 * x)) ; '(9 * x)
(extract 'before '* '(8 * y + 9 * x)) ; 8
(extract 'after  '* '(8 * y + 9 * x)) ; '(y + 9 * x)

(define (contains? symbol x)
  (cond ((or (empty? x) (not (list? x))) false)
	((eq? (car x) symbol) true)
	(else (contains? symbol (cdr x)))))

;; sum
(define (sum? x)
  (contains? '+ x))

(define (addend s)
  (extract 'before '+ s))

(define (augend s)
  (extract 'after '+ s))

;; Some unit tests
(addend '(7 * x + 5 * y + -3 * z)) ; '(7 * x)
(augend '(7 * x + 5 * y + -3 * z)) ; '(5 * y + -3 * z)

;; product
(define (product? x)
  (contains? '* x))

(define (multiplier p)
  (extract 'before '* p))

(define (multiplicand p)
  (extract 'after '* p))

;; exponentiation
(define (exponentiation? x)
  (contains? '** x))

(define (base e)
  (extract 'before '** e))
  
(define (exponent e)
  (extract 'after '** e))

;; We managed to solve this by changing only predicates and selectors, 
;; leaving constructors and the "deriv" procedure unchanged.

;; Some derivatives
(deriv '(x * y * 3 + (x + y + 2)) 'x) ; '((y * 3) + 1)

(define p1 
  '(-2 * x ** 3 + x + 7 * x ** 2 + -6 * x + 10))

(define dp1/dx
  (deriv p1 'x))

dp1/dx
; '((-2 * (3 * (x ** 2))) + (1 + ((7 * (2 * x)) + -6)))

;; How could we simplify the result?

;; First, we need to get rid of all the unnecessary parentheses.
;; Fringe procedure from 2.28 flattens the tree to a list of leaves:
(define (fringe tree)
  (cond ((null? tree) empty)
	((not (pair? tree)) (list tree))
	(else (append (fringe (car tree))
		      (fringe (cdr tree))))))

(fringe dp1/dx)
; '(5 * 4 * x ** 3 + -2 * 3 * x ** 2 + 7 * 2 * x + -6)

;; Splits the polynome into a list of operands of infix operator (op).
;; Example: (split-by '+ '(3 * x + 5 * y + 11)) -> '((3 * x) (5 * y) 11)
(define (split-by op polynome)
  (cond ((empty? polynome) empty)
	((not (list? polynome)) (list polynome))
	(else (append (list (extract 'before op polynome))
		      (split-by op (extract 'after op polynome))))))

(define (summands polynome)
  (split-by '+ polynome))

(define (factors polynome)
  (split-by '* polynome))

(factors (car (summands (fringe dp1/dx)))) ; '(-2 3 (x ** 2))

;; Simplification strategy:

;; First split the polynome into a list of summands, then split
;; each summand into a list of factors. Group the factors to numbers and
;; non-numbers. Multiply the numbers, and construct a list from the
;; resulting number and the non-numbers. Put "*" between the list elements
;; to form an infix multiplication expression. 

;; Some of these expressions could be just numbers. Group the list of 
;; multiplication expressions to numbers and non-numbers. Add the numbers,
;; and construct a list from the resulting number and non-numbers. Put "+" 
;; between the list elements to form an infix addition expression.

;; Interleaves the symbol of operation (op) between the list elements to form
;; an infix-style expression. Example: (infix '* '(5 x y)) -> '(5 * x * y).
(define (infix op lst)
  (cond ((empty? lst) empty)
	(else (append (list (first lst))
		      (let ((butfirst (infix op (rest lst))))
			(if (empty? butfirst)
			    empty
			    (cons op butfirst)))))))

(infix '+ '(12 (2 * w) (3 * y) x)) ; '(12 + (2 * w) + (3 * y) + x)

(define (infix-add lst)
  (infix '+ lst))

(define (infix-multiply lst)
  (infix '* lst))
  
(infix-add '(4 z w)) ; '(4 + z + w)
		      
;; Apply the operation (op) to the car of list
(define apply-car
  (λ (op lst) 
      (append (list (apply op (car lst))) 
	      (cadr lst))))

(define (apply-car-* lst)
  (apply-car * lst))

(define (apply-car-+ lst)
  (apply-car + lst))

(apply-car-+ '((3 5) ((x * y)))) ; '(8 (x * y))

;; Release the lone element from list confinement. Leave longer lists intact.
;; Example: (release-singleton '(42)) -> 42. 
(define release-singleton
  (λ (e) (if (= (length e) 1)
	     (car e)
	     e)))

;; Group the list elements to two sublists: numbers and non-numbers
(define (group lst)
  (cons (filter number? lst) 
	(list (filter (λ (n) (not (number? n))) 
		      lst))))

(group '(a 1 5 y 3 z 2 k)) ; '((1 5 3 2) (a y z k))

;; Helper procedure from 2.16. 
;; Example: (shift-left  '(2 0 7 3)) ; '(0 7 3 2)
(define (shift-left lst)
  (append (cdr lst) (list (car lst))))

;; The main simplification procedure
(define (simplify polynome)
  ((compose fringe
	    infix-add
	    shift-left  
	    apply-car-+
	    group)
   (map (compose release-singleton
		 infix-multiply
		 apply-car-*
		 group
		 factors)
	(summands polynome))))

;; The same polynome defined higher up:
p1 ; '(-2 * x ** 3 + x + 7 * x ** 2 + -6 * x + 10)

;; Derivative of p1 with respect to x:
dp1/dx ; '((-2 * (3 * (x ** 2))) + (1 + ((7 * (2 * x)) + -6)))

;; Without nested parentheses:
(fringe dp1/dx) ; '(-2 * 3 * x ** 2 + 1 + 7 * 2 * x + -6)

;; Result simplified:
(simplify (fringe dp1/dx)) ; '(-6 * x ** 2 + 14 * x + -5)
