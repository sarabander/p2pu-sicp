
;; From the book, extended
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
	((exponentiation? exp)                                       ; this
	 (make-product                                               ; is
	  (make-product (exponent exp)                               ; new
			(make-exponentiation (base exp)              ;
					     (sub1 (exponent exp)))) ;
	  (deriv (base exp) var)))                                   ;
	(else
	 (error "unknown expression type - DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

;; Added procedures for differentiating variables raised to powers
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base p) (cadr p))

(define (exponent p) (caddr p))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
	((=number? exponent 1) base)
	((and (number? base) (number? exponent)) (** base exponent))
	(else (list '** base exponent))))

(define ** expt)

;; Tests
(deriv '(** x 4) 'x) ; '(* 4 (** x 3))

(deriv '(+ (** x 3) (* 5 x)) 'x) ; '(+ (* 3 (** x 2)) 5)

(deriv '(+ 3 x x 4 7) 'x) ; 1 (can't handle sums with more than 2 arguments)
