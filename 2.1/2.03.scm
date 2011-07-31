
;; Depends on 2.02

;; Both definitions of make-rectangle below create rectangles 
;; with sides parallel to coordinate axes. 

;; First variant asks for lower-left and upper-right corner of the rectangle
(define (make-rectangle down-left up-right)
  (let ((up-left    (make-point (x-point down-left)
				(y-point up-right)))
	(down-right (make-point (x-point up-right)
				(y-point down-left))))
    (rect-4-corners down-left up-left up-right down-right)))

;; 2nd variant asks for diagonal running from upper-left to lower-right corner
(define (make-rectangle diagonal)
  (let* ((diag-start (start-segment diagonal))
	 (diag-end   (end-segment diagonal))
	 (up-left    diag-start)
	 (down-right diag-end)
	 (down-left  (make-point (x-point diag-start)
				 (y-point diag-end)))
	 (up-right   (make-point (x-point diag-end)
				 (y-point diag-start))))
    (rect-4-corners down-left up-left up-right down-right)))

;; Accepts four corners of the rectangle as input.
;; Returns 4 sides as segment pairs: ((left, right), (bottom, top))
(define (rect-4-corners down-left up-left up-right down-right)
  (let ((left   (make-segment down-left  up-left))
	(right  (make-segment down-right up-right))
	(bottom (make-segment down-left  down-right))
	(top    (make-segment up-left    up-right)))
    (cons (cons left right) (cons bottom top))))

(define (leftside-rectangle r)
  (caar r))

(define (rightside-rectangle r)
  (cdar r))

(define (bottomside-rectangle r)
  (cadr r))

(define (topside-rectangle r)
  (cddr r))

(define (segment-length s)
  (let ((x-length (abs (- (x-point (end-segment s))
			  (x-point (start-segment s)))))
	(y-length (abs (- (y-point (end-segment s))
			  (y-point (start-segment s))))))
    (sqrt (+ (sqr x-length) (sqr y-length)))))

(define (perimeter r)
  (* 2 (+ (segment-length (leftside-rectangle r))
	  (segment-length (bottomside-rectangle r)))))

(define (area r)
  (* (segment-length (leftside-rectangle r))
     (segment-length (bottomside-rectangle r))))

(define diagonal0 (make-segment (make-point 0 3) 
				(make-point 4 0)))

(segment-length diagonal0)

;; Using corner points
(define rect1 (make-rectangle (make-point 0 0)
			      (make-point 4 3)))

(perimeter rect1) ; => 14
(area rect1)      ; => 12

;; Using diagonal (must redefine make-rectangle first)
(define rect1 (make-rectangle diagonal0))

;; Third variant of make-rectangle uses two diagonals to construct
;; rectangle. First diagonal runs from up-left to down-right corner,
;; second from down-left to up-right corner.
(define (make-rectangle diag1 diag2)
  (if (and (equal-enough? (segment-length diag1) 
			  (segment-length diag2))
	   (close-enough? (midpoint-segment diag1)
			  (midpoint-segment diag2)))
      (let* ((up-left    (start-segment diag1))
	     (down-right (end-segment diag1))
	     (down-left  (start-segment diag2))
	     (up-right   (end-segment diag2)))
	(rect-4-corners down-left up-left up-right down-right))
      (printf "These diagonals don't describe a rectangle.\n")))

(define tolerance 0.001)

(define (equal-enough? a b)
  (< (abs (- a b)) tolerance))

(define (close-enough? point1 point2)
  (< (segment-length (make-segment point1 point2)) tolerance))

(define diagonal1 (make-segment (make-point 0 5/2)
				(make-point 0 -5/2)))

(define diagonal2 (make-segment (make-point -12/5 -7/10)
				(make-point 12/5 7/10)))

(define rect2 (make-rectangle diagonal1 diagonal2))

(perimeter rect2) ; => 14
(area rect2)      ; => 12

(segment-length (rightside-rectangle rect2)) ; => 4
(segment-length (topside-rectangle rect2))   ; => 3
