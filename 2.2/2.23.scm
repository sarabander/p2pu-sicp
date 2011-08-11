
;; From the book, modified
(for-each (lambda (x) (display x) (newline))
	  (list 57 321 88))

;; New implementation
(define (my-for-each f lst)
  (if (null? lst)
      (void)
      (begin
	(f (car lst))
	(my-for-each f (cdr lst)))))

(my-for-each (lambda (x) (display x) (newline))
	  (list 57 321 88))
