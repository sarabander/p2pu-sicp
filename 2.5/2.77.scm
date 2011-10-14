
;; We will put here all the involved definitions

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))
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
  ;; added
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method for these types - APPLY-GENERIC"
	   (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum - TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum - CONTENTS" datum)))
;;--------------------------------------------------

;; Starting with complex number z
(define z1 '(complex rectangular 3 . 4))

;; we want to find the magnitude
(magnitude z1)

;; This in turn calls
(apply-generic 'magnitude z1)

;; which queries the table with
(get 'magnitude '(complex))

;; It returns the procedure
magnitude

;; which is applied to the contents of z
(apply magnitude '((rectangular 3 . 4)))

;; producing a procedure call
(magnitude '(rectangular 3 . 4))

;; This again calls apply-generic, but with z1 where 'complex is stripped off
(apply-generic 'magnitude '(rectangular 3 . 4))

;; It queries the table with
(get 'magnitude '(rectangular))

;; and again returns the procedure
magnitude

;; which is applied to the contents of '(rectangular 3 . 4)
(apply magnitude '((3 . 4)))

;; producing a procedure call
(magnitude '(3 . 4))

;; But magnitude is defined in 'install-rectangular-package' as
(define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))

;; and returns 5.

;; So, apply-generic is invoked twice.
