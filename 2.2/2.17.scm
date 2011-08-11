
(define (last-pair lst)
  (let ((butfirst (cdr lst)))
    (if (null? butfirst)
	(list (car lst))
	(last-pair butfirst))))

(last-pair '(k l m n)) ; '(n)

(last-pair '()) ; error
