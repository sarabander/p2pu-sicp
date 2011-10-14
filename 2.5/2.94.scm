
;; Added GCD routines
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
  (define (gcd-terms a b)
    ;(printf "------------------------------------\n")
    ;(printf "a: ~a\n" (listpoly->mathpoly (tag (cons 'x a))))
    ;(printf "b: ~a\n" (listpoly->mathpoly (tag (cons 'x b))))
    (if (empty-termlist? b)
	a
	(gcd-terms b (remainder-terms a b))))
  (define (remainder-terms a b)
    (cadr (div-terms a b)))

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

(define (greatest-common-divisor a b)
  (apply-generic 'gcd a b))

;; Tests
(greatest-common-divisor 125 15) ; 5
(greatest-common-divisor 256 12) ; 4

(define p1 (make-polynomial
	    'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial 'x '((3 1) (1 -1))))

(listpoly->mathpoly p1) ; "x⁴ - x³ - 2x² + 2x"
(listpoly->mathpoly p2) ; "x³ - x"

;; gcd(p1, p2), the result:
(listpoly->mathpoly (greatest-common-divisor p1 p2)) ; "-x² + x"

;; We check the result using polynomial long division:

;; gcd(a, b) = gcd(x⁴ - x³ - 2x² + 2x, x³ - x), b is not 0, find remainder:

;; _ x⁴ - x³ - 2x² + 2x : x³ - x  =  x - 1 (we will throw quotient away)
;;   x⁴ - x²              ──────
;;   ───────                 Λ
;;     _ -x³ - x² + 2x       ╰── will be next a
;;       -x³ + x
;;       ────────
;;            -x² + x  <── remainder, will be next b

;; gcd(a, b) = gcd(x³ - x, -x² + x), b is still not 0, find another remainder:

;; _ x³ - x : -x² + x  =  -x - 1
;;   x³ - x²  ───────
;;   ───────        Λ
;;      _ x² - x    ╰── will be next a
;;        x² - x
;;        ──────
;;             0  <── remainder, will be next b

;; gcd(a, b) = gcd(-x² + x, 0), b is zero, so a is the gcd: -x² + x (correct!)
