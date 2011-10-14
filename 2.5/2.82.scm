
;; General version accepting arbitrary number of arguments
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (define (typecasts typelist) ; tries to find a typecast function for every
      (if (empty? typelist)      ; type recursively, returns list of functions
	  (error "Cannot coerce all arguments to same type")
	  (let* ((targettype (car typelist))
		 (proclist (map (λ (typepair) (apply get-coercion typepair))
				(map (λ (type) (cons type (list targettype)))
				     type-tags))))
	    (if (empty? (filter false? proclist))
		proclist
		(typecasts (cdr typelist))))))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (all-same? type-tags) ; added clause
	      (error "Matching types, but no operation available"
		     (list op type-tags))
	      (apply apply-generic (cons op (map (λ (f arg)
						    (f arg))
						 (typecasts type-tags)
						 args))))))))

;; This indeed attempts to apply the operation to arguments only when
;; the arguments are all the same type. As an example of legal operation 
;; with different argument types would be exponentiating a real number
;; with a complex number or vice versa. Current system remains blind
;; to this possibility.

;; Tests
(define (install-3-argument-add)
  (define (tag z) (attach-tag 'complex z))
  (define (add-complex z1 z2 z3)
    (make-from-real-imag (+ (real-part z1) (real-part z2) (real-part z3))
			 (+ (imag-part z1) (imag-part z2) (imag-part z3))))
  (put 'add '(complex complex complex)
       (lambda (z1 z2 z3) (tag (add-complex z1 z2 z3))))
  'done)

(install-3-argument-add)

(apply-generic 'add c2 c1 c1)     ; '(complex rectangular 7.0 . 9.0)
(apply-generic 'add c1 num1 num1) ; '(complex rectangular 27 . 4)
(apply-generic 'add num1 num2 c1) ; '(complex rectangular 19 . 4)
(apply-generic 'add num2 c2 c1)   ; '(complex rectangular 8.0 . 5.0)

((get-coercion 'scheme-number 'complex) num2)
((get-coercion 'scheme-number 'scheme-number) num1)
((get-coercion 'complex 'complex) c2)

;; Debugging constructs
(define (extract-types op . args)
  (map type-tag args))

(extract-types 'a num1 num2 c2)

(define cproclist
  (map (λ (typepair) (apply get-coercion typepair))
       (map (λ (type) (cons type (list 'complex)))
	    (extract-types 'a num1 num2 c2))))


(if (empty? (filter false? cproclist))
    cproclist
    'nonempty)

(apply apply-generic
       (cons 'add (map (λ (coercionproc arg)
			  (coercionproc (eval arg)))
		       cproclist
		       '(num1 num2 c1))))
