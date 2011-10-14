
;; Added pseudoremainder-terms, gcd-terms modified to use it.
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

  ;; New concept introduced to avoid fractional coefficients:
  (define (pseudoremainder-terms P Q)
    (let ((O1 (order (first-term P) P))
	  (O2 (order (first-term Q) Q))
	  (c (coeff (first-term Q))))
      (let ((integerizer (list (list 0 (expt c (+ 1 O1 (- O2)))))))
    (cadr (div-terms (mul-terms integerizer P) Q)))))

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
    ;(printf "L1: ~a\n" (listpoly->mathpoly (tag (cons 'x L1))))
    ;(printf "L2: ~a\n" (listpoly->mathpoly (tag (cons 'x L2))))
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
  'done)

(install-polynomial-package)

;; a. 
;; We introduce pseudoremainder-terms above to avoid fractional coefficients.

(listpoly->mathpoly (greatest-common-divisor Q1 Q2)) ; "1458x² - 2916x + 1458"
;; Integer coefficients, indeed! But not reduced.

;; b.
;; We modify gcd-terms above to reduce coefficients. 

(listpoly->mathpoly (greatest-common-divisor Q1 Q2)) ; "x² - 2x + 1"
;; Bingo!
