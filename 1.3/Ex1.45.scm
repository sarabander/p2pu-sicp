;; Definitions from the book

(define tolerance 0.00001)

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;; From 1.43

(define (repeated f times)
  (λ (x)
     (define (again n)
     (if (zero? n)
	 x
	 (f (again (sub1 n)))))
     (again times)))
;; ------------------------------

(define (cube x) (* x x x))

(define (cuberoot x)
  (fixed-point (average-damp (λ (y) (/ x (sqr y)))) 1.0))

(cuberoot 125)

(define (4th-root x)
  (fixed-point 
   ((repeated average-damp 2) (λ (y) (/ x (cube y)))) 1.0))

(4th-root 16)

(define (nth-root x n)
  (fixed-point 
   ((repeated average-damp 2) (λ (y) (/ x (expt y (sub1 n))))) 1.0))

(nth-root 81 4)
(nth-root 32 5)
(nth-root 128 7) ; works until 7th root with double average damping

(define (nth-root x n)
  (fixed-point 
   ((repeated average-damp 3) (λ (y) (/ x (expt y (sub1 n))))) 1.0))

(nth-root 256 8)
(nth-root 512 9)
(nth-root 1024 10)
(nth-root 32768 15) ; works until 15th root with triple average damping

;; Looks like we need at least d average dampings to take 
;; at most (2^(d+1) - 1)-th roots

;; If we name n = (2^(d+1) - 1) as the order of root, 
;; then             log(n + 1)
;;      d = ceiling(---------- - 1)
;;                     log 2

(define (nth-root x n)
  (let ((d (ceiling (- (/ (log (add1 n))
			  (log 2))
		       1))))
    (fixed-point 
     ((repeated average-damp d) 
      (λ (y) (/ x (expt y (sub1 n))))) 1.0)))

(nth-root 65536 16)
(nth-root (expt 2 1000) 1000)
(nth-root 81 1)
