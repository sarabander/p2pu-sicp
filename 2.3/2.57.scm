
;; Only augend and multiplicand needed modification

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

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

;; Extended
(define (augend s)
  (if (= (length s) 3)
      (caddr s)
      (cons '+ (cddr s)))) 

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

;; Extended
(define (multiplicand p)
  (if (= (length p) 3)
      (caddr p)
      (cons '* (cddr p))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

;; Tests
(deriv '(* x y (+ x 3)) 'x)
(deriv '(+ (* 4 x) (* 7 (** x 3)) (* x 5)) 'x)
(deriv '(* 4 x (** (+ x (* 6 y) 2) 3) z) 'y)
