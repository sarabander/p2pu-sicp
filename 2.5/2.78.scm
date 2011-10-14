
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method for these types - APPLY-GENERIC"
	   (list op type-tags))))))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
	(lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
	(lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
	(lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
	(lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
	(lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; Modified to use simple Scheme numbers
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
	((pair? datum) (car datum))
	(else (error "Bad tagged datum - TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
	((pair? datum) (cdr datum))
	(else (error "Bad tagged datum - CONTENTS" datum))))


;; Tests

(attach-tag 'rational '(3 4)) ; '(rational 3 4)
(attach-tag 'scheme-number 5) ; 5

(type-tag 3.54) ; 'scheme-number
(type-tag '(complex 2 7.6)) ; 'complex

(contents 3.54) ; 3.54
(contents '(complex 2 7.6)) ; '(2 7.6)
