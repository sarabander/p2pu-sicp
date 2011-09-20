
;; From the book
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

;; Added definitions:

;; Why don't we just filter one set through another?
(define (intersection-set set1 set2)
  (filter (λ (e) (element-of-set? e set2)) 
	  set1))

(define (union-set set1 set2)
  (cond ((empty? set1) set2)
	((empty? set2) set1)
	(else
	 (let ((subset (union-set (cdr set1) set2)))
	   (cond ((element-of-set? (car set1) subset) subset)
		 (else (cons (car set1) subset)))))))

;; Better version
(define (union-set set1 set2)
  (append (filter (λ (e) (not (element-of-set? e set2))) 
		  set1) 
	  set2))

(union-set '(1 4) '(1 2)) ; '(4 1 2)
(intersection-set '(1 3 4) '(1 2 3)) ; '(1 3)
