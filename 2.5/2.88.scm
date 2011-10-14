
;; Negation and subtraction are added to generic-arithmetic/polynomials.scm:

;; Negates all terms
(define (negate-termlist termlist)
  (if (empty? termlist)
      empty
      (let ((negator (if (list? (car termlist))
			 (λ (elem) (list (car elem) (- (cadr elem))))
			 -)))
	(map negator termlist))))

(put 'neg '(polynomial)
     (lambda (p) 
       (tag (cons (variable p) 
		  (negate-termlist (contents p))))))

(put 'sub '(polynomial polynomial)
     (lambda (p1 p2) (tag (add-poly p1 (contents (neg (tag p2)))))))

;; Tests
(neg poly1) ; '(polynomial x -5 2 0 -4 -2)
(neg poly2) ; '(polynomial y (4 -5) (3 2) (1 -4) (0 -2))
(neg poly3) ; remains the same
(neg poly4) ; remains the same

(sub poly1 poly1) ; '(polynomial x 0 0 0 0 0)
(sub poly2 poly2) ; '(polynomial y)
(sub poly3 poly2) ; '(polynomial y (4 -5) (3 2) (1 -4) (0 -2))
(sub poly4 poly1) ; '(polynomial x -5 2 0 -4 -2)
