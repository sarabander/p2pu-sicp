
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                            ;;
;;     This is Generic Arithmetic Package     ;;
;;     ----------------------------------     ;;
;;          (from chapter 2 of SICP)          ;;
;;                                            ;;
;;  To setup all, evaluate this entire file.  ;;   (works in Racket 5.0)
;;                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Hashtable for operations, plus backup to retain old versions
(define optable (make-hash))
(define optable-backup (make-hash))

;; Same for coercions
(define coercions (make-hash))
(define coercions-backup (make-hash))

;; Getters and setters
(define (get operation types)
  (hash-ref optable (list operation types) false))

(define (get-backup operation types)
  (hash-ref optable-backup (list operation types) false))

(define (put operation types procedure)
  (let ((exists (get operation types)))
    (if exists
	(and (put-backup operation types exists)
	     (hash-set! optable (list operation types) procedure))
	(hash-set! optable (list operation types) procedure))))

(define (put-backup operation types procedure)
  (let ((exists (get-backup operation types)))
    (if exists
	(hash-set! optable-backup 
		   (list operation types) 
		   (cons procedure exists))
	(hash-set! optable-backup
		   (list operation types)
		   (list procedure)))))

(define (get-coercion oldtype newtype)
  (hash-ref coercions (list oldtype newtype) false))

(define (get-coercion-backup oldtype newtype)
  (hash-ref coercions-backup (list oldtype newtype) false))

(define (put-coercion oldtype newtype procedure)
  (let ((exists (get-coercion oldtype newtype)))
    (if exists
	(and (put-coercion-backup oldtype newtype exists)
	     (hash-set! coercions (list oldtype newtype) procedure))
	(hash-set! coercions (list oldtype newtype) procedure))))

(define (put-coercion-backup oldtype newtype procedure)
  (let ((exists (get-coercion-backup oldtype newtype)))
    (if exists
	(hash-set! coercions-backup 
		   (list oldtype newtype) 
		   (cons procedure exists))
	(hash-set! coercions-backup
		   (list oldtype newtype)
		   (list procedure)))))


;; First version of apply-generic from the book
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method for these types* - APPLY-GENERIC"
	   (list op type-tags))))))

;; General package for primitive number types
(define (install-package type)
  (define (tag x)
    (attach-tag type x))
  (put 'add (list type type)
	(lambda (x y) (tag (+ x y))))
  (put 'sub (list type type)
	(lambda (x y) (tag (- x y))))
  (put 'mul (list type type)
	(lambda (x y) (tag (* x y))))
  (put 'div (list type type)
	(lambda (x y) (tag (/ x y))))
  (put 'square   (list type) sqr)
  (put 'sine     (list type) sin)
  (put 'cosine   (list type) cos)
  (put 'atangent (list type) atan)
  (put 'make type
	(lambda (x) (tag x)))
  'done)

(install-package 'scheme-number)
(install-package 'integer)
(install-package 'real)

;; Constructors
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (make-integer n)
  ((get 'make 'integer) n))
(define (make-real n)
  ((get 'make 'real) n))


;; Rational package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))
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


;; Complex number packages

;; Constructors
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Selectors
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atangent (div (imag-part z) (real-part z))))
  (define (make-from-mag-ang r a)
    (cons (* r (cosine a)) (* r (sine a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
	(lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
	(lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atangent y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
	(lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
	(lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
			 (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
			 (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
		       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
		       (sub (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
	(lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
	(lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
	(lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
	(lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
	(lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
	(lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(install-rectangular-package)
(install-polar-package)
(install-complex-package)


;; Type coercions
(define (install-coercion-package)
  ;; Coercions to oneself
  (define (scheme-number->scheme-number n) n)
  (define (integer->integer n) n)
  (define (rational->rational n) n)
  (define (real->real n) n)
  (define (complex->complex z) z)
  ;; Promotions
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (define (integer->rational n)
    (make-rational (contents n) 1))
  (define (rational->real r)
    (make-real (/ (numer r) (denom r) 1.0)))
  (define (real->complex n)
    (scheme-number->complex n))
  ;; Interface to the rest of the system
  (put-coercion 'scheme-number 'scheme-number 
		scheme-number->scheme-number)
  (put-coercion 'integer  'integer  integer->integer)
  (put-coercion 'rational 'rational rational->rational)
  (put-coercion 'real     'real     real->real)
  (put-coercion 'complex  'complex  complex->complex)
  ;; 
  (put-coercion 'scheme-number 'complex  scheme-number->complex)
  (put-coercion 'integer       'rational integer->rational)
  (put-coercion 'rational      'real     rational->real)
  (put-coercion 'real          'complex  real->complex)
  'done)

(install-coercion-package)


;; Equality package from 2.79
(define (equ? x y) (apply-generic 'equ? x y))

(define (install-equality-package)
  (define numer car)
  (define denom cdr)
  (define real-part car)
  (define imag-part cdr)
  (define magnitude car)
  (define angle cdr)
  (define myequal? (λ (x y) (zero? (- x y))))
  (put 'equ? '(scheme-number scheme-number) myequal?)
  (put 'equ? '(integer integer) myequal?)
  (put 'equ? '(real real) myequal?)
  (put 'equ? '(rational rational)
       (λ (x y) (and (zero? (- (numer x) (numer y)))
		     (zero? (- (denom x) (denom y))))))
  (put 'equ? '(complex complex)
       (λ (x y) (equ? x y)))
  (put 'equ? '(rectangular rectangular)
       (λ (x y) (and (zero? (- (real-part x) (real-part y)))
		     (zero? (- (imag-part x) (imag-part y))))))
  (put 'equ? '(polar polar)
       (λ (x y) (and (zero? (- (magnitude x) (magnitude y)))
		     (zero? (- (angle x) (angle y))))))
  'done)

(install-equality-package)


;; Zero package from 2.80
(define (=zero? x) (apply-generic '=zero? x))

(define (install-zero-package)
  (define numer car)
  (put '=zero? '(scheme-number)
       zero?)
  (put '=zero? '(rational)
       (λ (r) (zero? (numer r))))
  (put '=zero? '(complex)
       (λ (z) (zero? (magnitude z))))
  'done)

(install-zero-package)


;; Simple generic operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (square x) (apply-generic 'square x))

;; Trigonometry
(define (sine x)     (apply-generic 'sine x))
(define (cosine x)   (apply-generic 'cosine x))
(define (atangent x) (apply-generic 'atangent x))

;; Type tags
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

;; (define (attach-tag type-tag contents)
;;   (cons type-tag contents))

;; (define (type-tag datum)
;;   (if (pair? datum)
;;       (car datum)
;;       (if (number? datum)
;; 	  'real
;; 	  (error "Bad tagged datum - TYPE-TAG" datum))))

;; (define (contents datum)
;;   (if (pair? datum)
;;       (cdr datum)
;;       (if (number? datum)
;; 	  datum
;; 	  (error "Bad tagged datum - CONTENTS" datum))))

(define (all-same? symbols)
  (if (empty? (cdr symbols))
      true
      (and (eq? (car symbols) (cadr symbols))
	   (all-same? (cdr symbols)))))
