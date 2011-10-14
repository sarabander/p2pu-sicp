
;; See setup.scm in subdirectory 'generic-arithmetic' for hash-table setup and 
;; arithmetic packages. The numbers num1, num2, r1, r2, c1, etc. are defined 
;; in the file tests.scm. 

;; Helper function
(define (all-same? symbols)
  (if (empty? (cdr symbols))
      true
      (and (eq? (car symbols) (cadr symbols))
	   (all-same? (cdr symbols)))))

(all-same? '(complex complex complex))  ; true
(all-same? '(integer rational complex)) ; false

;; The original 
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(let ((t1->t2 (get-coercion type1 type2))
		      (t2->t1 (get-coercion type2 type1)))
		  (cond (t1->t2
			 (apply-generic op (t1->t2 a1) a2))
			(t2->t1
			 (apply-generic op a1 (t2->t1 a2)))
			(else
			 (error "No method for these types"
				(list op type-tags))))))
	      (error "No method for these types"
		     (list op type-tags)))))))

;; Coersion of type to itself
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
	       scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;; a. This results in infinite loop. 

;; b. It works when an operation is found for these argument types.

;; c. Infinite loop is prevented:

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (all-same? type-tags) ; added clause
	      (error "Matching types, but no operation available"
		     (list op type-tags))
	      (if (= (length args) 2)
		  (let ((type1 (car type-tags))
			(type2 (cadr type-tags))
			(a1 (car args))
			(a2 (cadr args)))
		    (let ((t1->t2 (get-coercion type1 type2))
			  (t2->t1 (get-coercion type2 type1)))
		      (cond (t1->t2
			     (apply-generic op (t1->t2 a1) a2))
			    (t2->t1
			     (apply-generic op a1 (t2->t1 a2)))
			    (else
			     (error "No method for these types"
				    (list op type-tags))))))
		  (error "No method for these types"
			 (list op type-tags))))))))

;; Tests
(apply-generic 'add num1 num2) ; '(scheme-number . 16)

(apply-generic 'add 
	       (make-complex-from-real-imag -1 4) 
	       (make-scheme-number 3))
; '(complex rectangular 2 . 4)

(apply-generic 'mod c1 c1)
; Matching types, but no operation available (mod (complex complex))

(div c1 c1) ; '(complex polar 1 . 0.0)

(add (contents c2) (contents c2))
; Matching types, but no operation available (add (polar polar))

(get 'add '(polar polar)) ; false

(sub r1 r2) ; '(rational -1 . 12)
