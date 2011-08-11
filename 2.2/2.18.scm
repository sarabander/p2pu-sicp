
;; Already defined in Racket
(reverse '(1 2 3 4)) ; '(4 3 2 1)

;; Define a new one
(define (reverse-list lst)
  (if (null? lst)
      empty
      (append (reverse-list (cdr lst)) (list (car lst)))))
	 
(reverse-list '(a b c d e)) ; '(e d c b a)
