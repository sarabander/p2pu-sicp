
(define (reverse-list lst)
  (if (null? lst)
      empty
      (append (reverse-list (cdr lst)) (list (car lst)))))

(define (deep-reverse lst)
  (cond ((null? lst) empty)
	((not (pair? lst)) lst)
	(else (append (deep-reverse (cdr lst)) 
		      (list (deep-reverse (car lst)))))))

(deep-reverse '(1 2 3 4)) ; '(4 3 2 1)

(deep-reverse '(1 2 ((3) (4 5 6) 7 ) (8 (((9) 10) 11) 12 (13 14)) 15))
; '(15 ((14 13) 12 (11 (10 (9))) 8) (7 (6 5 4) (3)) 2 1)

(define x (list (list 1 2) (list 3 4)))

(reverse-list x) ; '((3 4) (1 2))

(deep-reverse x) ; '((4 3) (2 1))
