
;; First, we draw ourselves a clear picture of the current type tower.
;; If the tower changes, we just need to call 'build-type-tower' again.

;; testnum is typed number, 
;; rank is type's "floor number", 
;; tower is a hashmap
(define (build-type-tower testnum rank tower)
  (let ((next-up (raise testnum))
	(type (type-tag testnum)))
    (hash-set! tower type rank)
    (if (eq? type (type-tag next-up))
	tower
	(build-type-tower next-up (add1 rank) tower))))

;; This probes the type hierarchy by successively raising the given
;; test number's type higher until top is reached. Highest type is
;; it's own supertype, e.g. (raise 'complex) -> 'complex. The procedure
;; returns a hashmap, where type is the key and height in type hierarchy
;; is the value.

(define type-tower
  (build-type-tower (make-integer 2) 1 (make-hash)))

type-tower ; '#hash((integer . 1) (rational . 2) (real . 3) (complex . 4))

(hash-ref type-tower 'integer false) ; 1
(hash-ref type-tower 'real false)    ; 3

;; 
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
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
		       ((not rank1) (error "No such type" type1))
		       ((not rank2) (error "No such type" type2))
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


;; Tests

;; Just checking a subprocedure from apply-generic:
(let ((raisable (make-integer 7))) 
    (define (raise-until targettype num)
      (if (eq? (type-tag num) targettype)
	  num
	  (raise-until targettype (raise num))))
    (raise-until 'real raisable))
; '(real . 7.0)

(add r1 (make-real 14.6))  ; '(real . 15.35)
(mul c2 c1 r2) ; procedure mul: expects 2 arguments, given 3
(apply-generic 'add r1 c1) ; '(complex rectangular 3.75 . 4)
(apply-generic 'sub (make-integer 9) r1) ; '(rational 33 . 4)
(apply-generic 'div (make-real 4.8) (make-integer -2)) ; '(real . -2.4)
(apply-generic 'add r1 c1 c2 r2) ; I need two arguments, given 4
(apply-generic 'mul (make-integer 3) (make-integer 7)) ; '(integer . 21)
