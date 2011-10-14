
;; We define these generic operations:

(define (square x)   (apply-generic 'square x))
(define (sine x)     (apply-generic 'sine x))
(define (cosine x)   (apply-generic 'cosine x))
(define (atangent x) (apply-generic 'atangent x))

;; Then add these rows to the primitive numbers package:

  (put 'square   (list type) sqr)
  (put 'sine     (list type) sin)
  (put 'cosine   (list type) cos)
  (put 'atangent (list type) atan)

;; And these to rational package:

  (put 'square '(rational) 
       (lambda (r) (sqr (/ (numer r) (denom r)))))
  (put 'sine '(rational) 
       (lambda (r) (sin (/ (numer r) (denom r)))))
  (put 'cosine '(rational) 
       (lambda (r) (cos (/ (numer r) (denom r)))))
  (put 'atangent '(rational) 
       (lambda (r) (atan (/ (numer r) (denom r)))))

;; And do following substitutions in complex arithmetic packages:

;; +    -> add
;; -    -> sub
;; *    -> mul
;; /    -> div

;; sin  -> sine
;; cos  -> cosine
;; atan -> atangent

;; These changes are made in file generic-arithmetic/setup.scm.
