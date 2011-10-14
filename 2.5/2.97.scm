
;; Added procedures to reduce fractions
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p)) ; 

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; representation of terms and termlists
  (define (adjoin-term term term-list)
    (if (and (list? term) (=zero? (coeff term)))
	term-list
	(cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) 
    (or (null? term-list) (list-of-zeros? term-list)))
  (define (make-term type termorder termcoeff) 
    (if (eq? type 'dense)
	termcoeff
	(list termorder termcoeff)))
  (define (termtype term)
    (if (list? term) 'sparse 'dense))
  (define (order term partial-termlist) 
    (if (list? term)
	(car term)
	(sub1 (length partial-termlist))))
  (define (coeff term) 
    (if (list? term)
	(cadr term)
	term)) 
  ;; Greatest common divisor of polynomials
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (gcd-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var - GCD-POLY"
	       (list p1 p2))))

  ;; Original:
  ;; (define (gcd-terms a b)
  ;;   (if (empty-termlist? b)
  ;; 	a
  ;; 	(gcd-terms b (remainder-terms a b))))

  ;; Uses pseudoremainder-terms and removes common factors from coefficients:
  (define (gcd-terms a b)
    (if (empty-termlist? b)
	(let ((gcd-of-coeffs (apply gcd (map coeff a))))
	  (map (λ (term) (list (car term) 
			       (/ (cadr term) gcd-of-coeffs)))
	       a))
	(gcd-terms b (pseudoremainder-terms a b))))

  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  ;; New concepts introduced to avoid fractional coefficients:
  (define (pseudoquotient-terms P Q)
    (pseudodivision-terms car P Q))  
  (define (pseudoremainder-terms P Q)
    (pseudodivision-terms cadr P Q))
  (define (pseudodivision-terms f P Q)
    (let ((O1 (order (first-term P) P))
	  (O2 (order (first-term Q) Q))
	  (c (coeff (first-term Q))))
      (let ((integerizer (list (list 0 (expt c (+ 1 O1 (- O2)))))))
    (f (div-terms (mul-terms integerizer P) Q)))))

  (define (reduce-poly n d)
    (if (same-variable? (variable n) (variable d))
	(let ((n-d-reduced (reduce-terms (term-list n)
					 (term-list d))))
	  (cons (make-poly (variable n) (car  n-d-reduced))
		(make-poly (variable n) (cadr n-d-reduced))))
	(error "Polys not in same var - ADD-POLY"
	       (list p1 p2))))
  
  ;; Oh Lords of Lambda, I have sinned! 
  ;; I've penetrated the abstraction barrier!
  ;; (Must repent and rewrite the procedure thrice.)
  (define (reduce-terms n d)
    (let ((g (gcd-terms n d)))
      (let ((O1 (max (order (first-term n) n)
		     (order (first-term d) d)))
	    (O2 (order (first-term g) g))
	    (c (coeff (first-term g))))
	(let ((integerizer (list (list 0 (expt c (+ 1 O1 (- O2)))))))
	  (let ((n-integerized (mul-terms integerizer n))
		(d-integerized (mul-terms integerizer d)))
	    (let ((n/g (car (div-terms n-integerized g)))
		  (d/g (car (div-terms d-integerized g))))
	      (let ((gcd-of-coeffs 
		     (apply gcd (map coeff (append n/g d/g)))))
		(let ((divide-by-gcd 
		       (λ (term) (list (car term) (/ (cadr term) 
						     gcd-of-coeffs)))))
		  (let ((n-reduced (map divide-by-gcd n/g))
			(d-reduced (map divide-by-gcd d/g)))
		    (list n-reduced d-reduced))))))))))

  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (add-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var - ADD-POLY"
	       (list p1 p2))))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1)) 
		 (t2 (first-term L2)))
	     (cond ((> (order t1 L1) (order t2 L2))
		    (adjoin-term
		     t1 (add-terms (rest-terms L1) L2)))
		   ((< (order t1 L1) (order t2 L2))
		    (adjoin-term
		     t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term
		     (make-term (termtype t1)
				(order t1 L1)
				(add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1)
				(rest-terms L2)))))))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var - MUL-POLY"
	       (list p1 p2))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	(the-empty-termlist)
	(add-terms (mul-term-by-all-terms (first-term L1) 
					  (order (first-term L1) L1) 
					  L2)
		   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 t1-order L)
    (if (empty-termlist? L)
	(the-empty-termlist)
	(let ((t2 (first-term L)))
	  (adjoin-term
	   (make-term (termtype t1)
		      (+ t1-order (order t2 L))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 
				  t1-order 
				  (rest-terms L))))))
  ;; Division
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(let ((divided-terms (div-terms (sort-termlist (term-list p1))
					(sort-termlist (term-list p2)))))
	  (list   
	   (make-poly (variable p1) (car divided-terms))
	   (make-poly (variable p1) (cadr divided-terms))))
	(error "Polys not in same var - MUL-POLY"
	       (list p1 p2))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
	(list (the-empty-termlist) (the-empty-termlist))
	(let ((t1 (first-term L1))
	      (t2 (first-term L2)))
	  (if (> (order t2 L2) (order t1 L1))
	      (list (the-empty-termlist) L1)
	      (let ((new-c (div (coeff t1) (coeff t2)))
		    (new-o (- (order t1 L1) (order t2 L2))))
		(let* ((newterm (list (make-term 'sparse new-o new-c)))
		       (rest-of-result
			(let* ((subtractor (mul-terms newterm L2))
			       (difference (sort-termlist 
					    (add-terms L1 
						       (negate-termlist 
							subtractor)))))
			  (if (empty? difference)
			      (list (the-empty-termlist)
				    (the-empty-termlist))
			      (div-terms difference L2)))))
		  (list
		   (sort-termlist (add-terms newterm 
					     (car rest-of-result))) 
		   (cadr rest-of-result))))))))

  ;; Sorts sparse termlist by term's order (descending)
  (define (sort-termlist L)
    (sort L #:key car >))
  ;; Converts dense termlist to sparse, leaves already sparse list intact
  (define (convert-to-sparse termlist)
    (if (empty? termlist)
	empty
	(let ((first-elem (car termlist)))
	  (if (and (number? first-elem) (zero? first-elem))
	      (convert-to-sparse (cdr termlist))
	      (cons (list (order first-elem termlist) (coeff first-elem))
		    (convert-to-sparse (cdr termlist)))))))
  ;; Converts dense polynomial to sparse
  (define (to-sparse polynomial)
    (cons (variable polynomial) 
	  (convert-to-sparse (term-list polynomial))))
  ;; Negates all terms
  (define (negate-termlist termlist)
    (if (empty? termlist)
	empty
	(let ((negator (if (list? (car termlist))
			   (λ (elem) (list (car elem) (- (cadr elem))))
			   -)))
	  (map negator termlist))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
	(lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
	(lambda (p1 p2) (tag (add-poly p1 (contents (neg (tag p2)))))))
  (put 'mul '(polynomial polynomial)
	(lambda (p1 p2) (tag (mul-poly (to-sparse p1) 
				       (to-sparse p2)))))
  (put 'div '(polynomial polynomial)
	(lambda (p1 p2) 
	  (let ((quotient+remainder (div-poly (to-sparse p1) 
					      (to-sparse p2))))
	    (list (tag (car  quotient+remainder))
		  (tag (cadr quotient+remainder))))))
  (put 'neg '(polynomial)
       (lambda (p) 
	 (tag (cons (variable p) 
		    (negate-termlist (contents p))))))
  (put '=zero? '(polynomial)
       (λ (p) (list-of-zeros? (contents p))))
  (put 'gcd '(polynomial polynomial)
       (λ (p1 p2) (tag (gcd-poly (to-sparse p1) 
				 (to-sparse p2)))))
  (put 'gcd '(scheme-number scheme-number) gcd)
  (put 'make 'polynomial
	(lambda (var terms) (tag (make-poly var terms))))
  (put 'reduce '(scheme-number scheme-number) reduce-integers)
  (put 'reduce '(polynomial polynomial)
       (λ (n d) 
	  (let ((reduced-p (reduce-poly n d)))
	    (cons (tag (car reduced-p))
		  (tag (cdr reduced-p))))))
  'done)

(install-polynomial-package)


(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (make-rat n d) (reduce n d))

  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
		   (mul (numer y) (denom x)))
	      (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
		   (mul (numer y) (denom x)))
	      (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
	      (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (div (numer x) (denom y))
	      (div (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
	(lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
	(lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
	(lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
	(lambda (x y) (tag (div-rat x y))))
  (put 'square '(rational) 
       (lambda (r) (sqr (/ (numer r) (denom r)))))
  (put 'sine '(rational) 
       (lambda (r) (sin (/ (numer r) (denom r)))))
  (put 'cosine '(rational) 
       (lambda (r) (cos (/ (numer r) (denom r)))))
  (put 'atangent '(rational) 
       (lambda (r) (atan (/ (numer r) (denom r)))))
  (put 'make 'rational
	(lambda (n d) (tag (make-rat n d))))
  (put 'numer '(rational)
	(lambda (r) (numer r)))
  (put 'denom '(rational)
	(lambda (r) (denom r)))
  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (numer r)
  (apply-generic 'numer r))
(define (denom r)
  (apply-generic 'denom r))

(define (reduce n d)
  (apply-generic 'reduce n d))

;; Tests
(make-rational 12 81) ; '(rational 4 . 27)

(define p1 (make-polynomial 'x '((1 1) (0 1))))
(listpoly->mathpoly p1) ; "x + 1"

(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(listpoly->mathpoly p2) ; "x³ - 1"

(define p3 (make-polynomial 'x '((1 1))))
(listpoly->mathpoly p3) ; "x"

(define p4 (make-polynomial 'x '((2 1) (0 -1))))
(listpoly->mathpoly p4) ; "x² - 1"

(define rf1 (make-rational p1 p2))
rf1 ; '(rational (polynomial x (1 -1) (0 -1)) polynomial x (3 -1) (0 1))
(listpoly->mathpoly (numer rf1)) ; "-x - 1"
(listpoly->mathpoly (denom rf1)) ; "-x³ + 1"

(define rf2 (make-rational p3 p4))
rf2 ; '(rational (polynomial x (1 1)) polynomial x (2 1) (0 -1))
(listpoly->mathpoly (numer rf2)) ; "x"
(listpoly->mathpoly (denom rf2)) ; "x² - 1"

(define rf1+rf2 (add rf1 rf2))
rf1+rf2
;; '(rational
;;   (polynomial x (3 -1) (2 -2) (1 -3) (0 -1))
;;   polynomial
;;   x
;;   (4 -1)
;;   (3 -1)
;;   (1 1)
;;   (0 1))

(listpoly->mathpoly (numer rf1+rf2)) ; "-x³ - 2x² - 3x - 1"
(listpoly->mathpoly (denom rf1+rf2)) ; "-x⁴ - x³ + x + 1"

;; Correct! 

;; (Not to be picky, but we could multiply numerator and denominator by -1.)
