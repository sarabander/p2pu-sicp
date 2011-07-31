(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; one = (add-1 zero) = (lambda (f) (lambda (x) (f ((zero f) x)))) =
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))) =
;; (lambda (f) (lambda (x) (f ((lambda (x) x) x)))) =
;; (lambda (f) (lambda (x) (f x))) 

(define one (lambda (f) (lambda (x) (f x))))

;; two = (add-1 one) = (lambda (f) (lambda (x) (f ((one f) x)))) =
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x)))) =
;; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x)))) =
;; (lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (plus m n)
  (λ (f) (λ (x) ((repeated f (+ m n)) x))))

(define (repeated f times)
  (λ (x)
     (define (again n)
     (if (zero? n)
	 x
	 (f (again (sub1 n)))))
     (again times)))

((two add1) 0) ; => 2

(((plus 3 4) add1) 3) ; => 10
