
;; We take away car of first set, compare it to car of second set.
;; If it's less, we append it to third set. If it's equal, 
;; we ignore it and append car of second set to third set. And so on.
;; Number of steps grows as O(n), where n is cardinality of the bigger set.

(define (union-set set1 set2)
  (define (iter s1 s2 s3)
    (cond ((empty? s1) (append s3 s2))
	  ((empty? s2) (append s3 s1))
	  ((< (car s1) (car s2)) 
	   (iter (cdr s1) 
		 s2 
		 (append s3 (list (car s1)))))
	  ((= (car s1) (car s2)) 
	   (iter (cdr s1) 
		 (cdr s2) 
		 (append s3 (list (car s2)))))
	  ((> (car s1) (car s2)) 
	   (iter s1
		 (cdr s2) 
		 (append s3 (list (car s2)))))
	  (else (error "How did you get here?" s1 s2 s3))))
  (iter set1 set2 empty))

;; Tests
(union-set '() '())        ; '()
(union-set '(1 5) '(-3 8)) ; '(-3 1 5 8)
(union-set '() '(4 7 12))  ; '(4 7 12)
(union-set '(-9 2) '(2))    ; '(-9 2)
(union-set '(6 8) '(5 7))  ; '(5 6 7 8)
