
;; Definitions from the book
(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
;; -------------------------------------------

(define (cubic a b c)
  (λ (x)
     (+ (cube x)
	(* a (sqr x))
	(* b x)
	c)))

(newtons-method (cubic 3 -5 2) 1) ; => -4.278039570812626

;; Check
((cubic 3 -5 2) -4.278039570812626) ; => 2.4304824819409987e-10

(newtons-method (cubic 1 -3 -6) 1) ; => 2.000000000007438

;; Check
((cubic 1 -3 -6) 2) ; => 0
