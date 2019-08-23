
;; Recursive process
(define (f1 n)
  (cond ((< n 3) n)
	(else (+ (f1 (- n 1))
		 (* 2 (f1 (- n 2)))
		 (* 3 (f1 (- n 3)))))))


; why do we need multiplication here ?
; f(n) = f(n−1)+f(n−2)+f(n−3), if n ≥ 3.
; Why don't just write like this:
;; (define (f n) 
;;   (if (< n 3)
;;   n
;;   (+ 
;;     (f (- n 1))
;;     (f (- n 2))
;;     (f (- n 3)))))

;; Test 
(f1 30)
;; takes 2 seconds

;; Iterative process
(define (f2 n)
  (define (iter a b c count) ; zeroth, first, second, counter
    (cond ((= count 0) a)
	  (else (iter b 
		      c 
		      (+ c (* 2 b) (* 3 a)) 
		      (- count 1)))))
  (iter 0 1 2 n))

;; The same, with slight change to avoid 2 unnecessary calculations of c
(define (f2 n)
  (define (iter a b c count) ; zeroth, first, second, counter
    (cond ((< count 0) n)
	  ((= count 0) c)
	  (else (iter b 
		      c 
		      (+ c (* 2 b) (* 3 a)) 
		      (- count 1)))))
  (iter 0 1 2 (- n 2)))

;; Tests
(f2 30)

(- (f1 20)
   (f2 20))

;; Evaluates instantly
(f2 200)

;; (f1 200) takes eternity
