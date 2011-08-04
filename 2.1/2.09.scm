
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (let ((p1 (- (lower-bound x) (lower-bound y)))
	(p2 (- (lower-bound x) (upper-bound y)))
	(p3 (- (upper-bound x) (lower-bound y)))
	(p4 (- (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

(define (width interval)
  (/ (- (upper-bound interval) 
	(lower-bound interval))
     2.0))

(define interval1 (make-interval 4.4 4.9))
(define interval2 (make-interval 7.2 7.6))

(define interval3 (make-interval 3.4 3.9))
(define interval4 (make-interval 9.2 9.6))

(width interval1)
(width interval3) ; => both 0.25
(width interval2)
(width interval4) ; => both 0.2

;; Width in addition and subtraction depends only on argument interval widths
(width (add-interval interval1 interval2)) ; => 0.45
(width (sub-interval interval1 interval2)) ; => 0.45

(width (add-interval interval3 interval4)) ; => 0.45
(width (sub-interval interval3 interval4)) ; => 0.45

;; Width in multiplication and division depends also on interval boundaries
(width (mul-interval interval1 interval2)) ; => 2.78
(width (div-interval interval1 interval2)) ; => 0.051

(width (mul-interval interval3 interval4)) ; => 3.08
(width (div-interval interval3 interval4)) ; => 0.035

