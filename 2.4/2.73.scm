
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	(else
	 ((get 'deriv (operator exp))
	  (operands exp)
	  var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;; a. The separate differentiation rules were replaced by single dispatch.
;; It extracts the operator from expression, and looks up the row with
;; 'deriv and the operator from the table. It then applies the found
;; derivation procedure to operands, using var as differentiation variable.

;; number? and variable? can't be assimilated because they deal with the
;; case when the expression is an atom, not a list. The get procedure
;; needs a list to work.

;; b. 

;; For two operands. Needs generalizing.

(define (deriv-sum operands var)
  (make-sum (deriv (first  operands) var)
	    (deriv (second operands) var)))

(define (deriv-product operands var)
  (make-sum
   (make-product (first operands)
		 (deriv (second operands) var))
   (make-product (deriv (first operands) var)
		 (second operands))))


(put 'deriv '+ deriv-sum)

(put 'deriv '* deriv-product)

;; c.

(define (deriv-cosine operand var)
  (- (make-product ('sin (first operand))
		   (deriv (first operand)))))

(put 'deriv 'cos deriv-cosine)

;; d. We only need to change the definition of get and put.
