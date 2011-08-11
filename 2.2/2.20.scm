
;; Helper function
(define (myfilter pred lst)
  (if (null? lst)
      empty
      (append (if (pred (car lst)) 
		  (list (car lst))
		  empty)
	      (myfilter pred (cdr lst)))))

;; Test
(myfilter even? '(1 2 3 4 5 6)) ; '(2 4 6)

;; First version
(define (same-parity x . y)
  (if (odd? x)
      (cons x (myfilter odd?  y))
      (cons x (myfilter even? y))))

;; Second version
(define same-parity
  (λ w 
     (if (null? w)
	 empty
	 (let ((first (car w))
	       (rest  (cdr w)))
	   (if (odd? first)
	       (cons first (myfilter odd?  rest))
	       (cons first (myfilter even? rest)))))))
     
;; Test
(same-parity 1 2 3 4 5 6 7) ; '(1 3 5 7)
(same-parity 2 3 4 5 6 7)   ; '(2 4 6)
(same-parity 2) ; '(2)
(same-parity)   ; error with first, '() with second version
