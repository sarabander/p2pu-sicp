
(define (reverse-list lst)
  (if (null? lst)
      empty
      (append (reverse-list (cdr lst)) (list (car lst)))))

(define (deep-reverse1 lst)
    (if (pair? lst)
        (append (deep-reverse1 (cdr lst))
                (list (deep-reverse1 (car lst))))
        lst))

(define (deep-reverse2 lst)
    (if (pair? lst)
        (reverse (map deep-reverse2 lst))
        lst))

(define deep-reverse deep-reverse1)

(deep-reverse '(1 2 3 4)) ; '(4 3 2 1)

(deep-reverse '(1 2 ((3) (4 5 6) 7 ) (8 (((9) 10) 11) 12 (13 14)) 15))
; '(15 ((14 13) 12 (11 (10 (9))) 8) (7 (6 5 4) (3)) 2 1)

(define x (list (list 1 2) (list 3 4)))

(reverse-list x) ; '((3 4) (1 2))

(deep-reverse x) ; '((4 3) (2 1))
