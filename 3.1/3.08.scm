
(define f
  (let ((internal 0)
	(calls 0))
    (lambda (arg)
      (let ((previous internal))
	(set! calls (+ calls 1))
	(if (= calls 2)
	    (begin (set! internal 0)
		   (set! calls 0))
	    (set! internal arg))
	previous))))

(+ (f 0) (f 1)) ; 1
(+ (f 1) (f 0)) ; 0
;; MIT-Scheme evaluates from right to left.

(+ (f 0) (f 1)) ; 0
(+ (f 1) (f 0)) ; 1
;; Racket evaluates from left to right.
