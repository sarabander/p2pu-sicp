
(define (project num)
  (apply-generic 'project num))

(define (install-project-package)
  (define (project-complex z)
    (make-real (real-part z)))
  (define (project-real r)
    (let ((rational (inexact->exact r)))
      (make-rational (numerator   rational) 
		     (denominator rational))))
  (define (project-rational rat)
    (make-integer (round (/ (car rat) (cdr rat)))))
  (define (project-integer int)
    (make-integer int))
  (put 'project '(complex)  project-complex)
  (put 'project '(real)     project-real)
  (put 'project '(rational) project-rational)
  (put 'project '(integer)  project-integer)
  'done)

(install-project-package)

(define (drop num)
  (define (simplify higher lower)
    (let ((raised-lower (raise lower)))
      (cond ((and (eq? (type-tag raised-lower) (type-tag higher)) 
		  (equ? raised-lower higher))
	     (simplify lower (project lower)))
	    (else higher))))
  (simplify num (project num)))

;; A set of binary operations in the system.
;; Will be used in apply-generic to see if we are doing arithmetic operation.
;; If so, the drop procedure can be applied to simplify the returned result.
;; This is just a quick hack. Cleaner solution is needed.
(define arithmetic (make-hash))
(hash-set! arithmetic 'add true)
(hash-set! arithmetic 'sub true)
(hash-set! arithmetic 'mul true)
(hash-set! arithmetic 'div true)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (if (hash-ref arithmetic op false)          ; simplify if op is
	      (drop (apply proc (map contents args))) ; add, sub, mul or div
	      (apply proc (map contents args)))
	  (if (all-same? type-tags) 
	      (error "Matching types, but no operation available"
		     (list op type-tags))
	      (if (= (length args) 2)
		  (let ((type1 (car type-tags))
			(type2 (cadr type-tags))
			(a1 (car args))
			(a2 (cadr args))) ; major changes below this line
		    (let ((rank1 (hash-ref type-tower type1 false))
			  (rank2 (hash-ref type-tower type2 false)))
		      (cond 
		       ((not rank1) (error "No such type (1)" type1))
		       ((not rank2) (error "No such type (2)" type2))
		       (else
			(define (raise-until targettype num)
			  (if (eq? (type-tag num) targettype)
			      num
			      (raise-until targettype (raise num))))
			(if (< rank1 rank2)
			    (let ((raised-a1 (raise-until type2 a1)))
			      (apply-generic op raised-a1 a2))
			    (let ((raised-a2 (raise-until type1 a2)))
			      (apply-generic op a1 raised-a2)))))))
		  (error "I need two arguments, given" (length args))))))))


;;Tests  
(project c1) ; '(real . 3)
(project (make-real 0.625)) ; '(rational 5 . 8)
(project (make-real 17))    ; '(rational 17 . 1)
(project r1) ; '(integer . 1)
(project (make-rational 7 2)) ; '(integer . 4)
(project (make-integer 16))   ; '(integer . 16)

(equ? c3 (raise (project c3))) ; true
(equ? c1 (raise (project c1))) ; false
(equ? r1 (raise (project r1))) ; false
(equ? r3 (raise (project r3))) ; true
(equ? real1 (raise (project real1))) ; true
(equ? real2 (raise (project real2))) ; true 
; (limited precision floating point numbers can always 
; be converted to exactly equal rational numbers)

(equ? real1 (raise (raise (project (project real1))))) ; false
(equ? real3 (raise (raise (project (project real3))))) ; true

(equ? (raise '(integer . 10)) (make-rational 10 1)) ; true
(equ? (raise '(integer . 10)) '(integer . 10))
; No method for these types - APPLY-GENERIC (equ? (rational integer))

(drop (make-integer 72))   ; '(integer . 72)
(drop (make-rational 6 1)) ; '(integer . 6)
(drop (make-rational 7 2)) ; '(rational 7 . 2)
(drop c3)    ; '(rational 5 . 2)
(drop real3) ; '(integer . 14)
(drop real1) ; '(rational 21 . 8)

(inexact->exact 2.625) ; 21/8

(add c3 c3)    ; '(integer . 5)
(add real3 c3) ; '(rational 33 . 2)
(apply-generic 'add real1 real3) ; '(rational 133 . 8)
(project c2) ; '(real . 1.0)

(mul c2 c2) ; No such type (1) rectangular 
; (can't drop polar complex numbers: bug in drop procedure)

(sub c2 c2) ; '(integer . 0)
(contents (add c3 c3)) ; 5
(mul real3 real3) ; '(integer . 196)
(div r1 r1) ; '(integer . 1)
(mul real1 (make-integer 2)) ; '(rational 21 . 4)
