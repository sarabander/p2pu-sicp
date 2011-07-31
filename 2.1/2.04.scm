
(define (mycons x y)
  (lambda (m) (m x y)))

(define (mycar z)
  (z (lambda (p q) p)))

(mycar (mycons 3 12)) ; => 3

;; mycons returns a procedure that expects a procedure as its input.
;; mycar expects a procedure as input and applies it to another procedure.

;; By calling (mycons 3 12) we get anonymous procedure (λ (m) (m 3 12)).
;; By calling (mycar (mycons 3 12)) we produce the following expression:
;; ((λ (m) (m 3 12)) (λ (p q) p)), which in turn produces another one:
;; ((λ (p q) p) 3 12). This returns its first argument and discards second.

;; Like mycar, but the λ-expression returns 2nd argument and discards 1st.
(define (mycdr z)
  (z (lambda (p q) q)))

(mycdr (mycons 3 12)) ; => 12
