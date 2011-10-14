
;; Modified to use generic operations (add, mul, etc.). 
;; Reducing of rationals to lowest terms is prevented.
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  ;; (define (make-rat n d)
  ;;   (let ((g (gcd n d)))
  ;;     (cons (/ n g) (/ d g))))
  (define (make-rat n d)
      (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
		   (mul (numer y) (denom x)))
	      (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
		   (mul (numer y) (denom x)))
	      (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
	      (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (div (numer x) (denom y))
	      (div (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
	(lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
	(lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
	(lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
	(lambda (x y) (tag (div-rat x y))))
  (put 'square '(rational) 
       (lambda (r) (sqr (/ (numer r) (denom r)))))
  (put 'sine '(rational) 
       (lambda (r) (sin (/ (numer r) (denom r)))))
  (put 'cosine '(rational) 
       (lambda (r) (cos (/ (numer r) (denom r)))))
  (put 'atangent '(rational) 
       (lambda (r) (atan (/ (numer r) (denom r)))))
  (put 'make 'rational
	(lambda (n d) (tag (make-rat n d))))
  (put 'numer '(rational)
	(lambda (r) (numer r)))
  (put 'denom '(rational)
	(lambda (r) (denom r)))
  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (numer r)
  (apply-generic 'numer r))
(define (denom r)
  (apply-generic 'denom r))

;; Test polynomials:
(define p1 (make-polynomial 'x '((2 1)(0 1))))
(define p2 (make-polynomial 'x '((3 1)(0 1))))

(define rf (make-rational p2 p1))
; '(rational (polynomial x (3 1) (0 1)) polynomial x (2 1) (0 1))

(numer rf) ; '(polynomial x (3 1) (0 1))
(denom rf) ; '(polynomial x (2 1) (0 1))

(add rf rf)
; '(rational
;   (polynomial x (5 2) (3 2) (2 2) (0 2))
;   polynomial
;   x
;   (4 1)
;   (2 2)
;   (0 1))

;; Indeed, fraction is not reduced.
