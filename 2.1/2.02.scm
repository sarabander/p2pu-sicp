
(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment s)
  (let* ((start (start-segment s))
	 (end   (end-segment s))
	 (x1 (x-point start))
	 (y1 (y-point start))
	 (x2 (x-point end))
	 (y2 (y-point end)))
    (make-point (average x1 x2)
		(average y1 y2))))

;; From the book, modified
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

;; Make points and a segment

(define a1 (make-point 4 2))
(define b1 (make-point 10 -12))

(define seg1 (make-segment a1 b1))

;; Find midpoint
(print-point (midpoint-segment seg1)) ; => (7,-5) correct
