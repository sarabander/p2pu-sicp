
(define (my-equal? a b)
  (cond ((and (empty? a) 
	      (empty? b))
	 true)
	((and (number? a)
	      (number? b)
	      (eq? a b))
	 true)
	((and (symbol? a)
	      (symbol? b)
	      (eq? a b))
	 true)
	((and (list? a)
	      (list? b)
	      (my-equal? (car a) (car b))
	      (my-equal? (cdr a) (cdr b)))
	 true)
	(else 
	 false)))

(my-equal? '(this is a list) '(this is a list))   ; true
(my-equal? '(this is a list) '(this (is a) list)) ; false

(my-equal? 'e 'r) ; false
(my-equal? 'w 'w) ; true

(my-equal? 7 12)  ; false
(my-equal? 3 +3)  ; true

(my-equal? '(23 4 (5 (72)) (14)) '(23 4 (5 (72)) (14))) ; true
(my-equal? '(23 4 (5 (70)) (14)) '(23 4 (5 (72)) (14))) ; false
