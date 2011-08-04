
;; Let x and y designate the two intervals we wish to multiply. 
;; We will use these prefixes:

;; l - lower-bound (e.g. lx is the lower-bound of x)
;; u - upper-bound

;; Interval endpoints could be positive or negative. There are 16 
;; combinations when taking two intervals with four endpoints. Some of
;; these are not "legal" in the sense that one or both of the intervals
;; have their upper bound smaller than lower bound (+ − combination). 
;; We can immediately dismiss seven of such illegal cases. Remains nine.

;; We pick the two multiplications to produce result's lower and upper bound 
;; by carefully considering, which of them gives the smallest and which the 
;; largest value. Only one combination of signs gives an ambiguous result, 
;; so we need four multiplications, and must use min and max functions.

;; Next table illustrates the multiplication pattern.

;;
;;             x         y              multiplicands
;;           -----     -----           ---------------
;;  legal?   lx ux     ly uy    case   lower     upper
;;  ------   -- --     -- --    ----   -----     -----
;; 
;;            ╭───u────╮
;;     ✓     (−)(−)   (−)(−)     1.    [ux⋅uy,  lx⋅ly]
;;               ╰────l───╯
;;
;;            ╭───u────╮
;;     ✓     (−)(−)   (−)(+)     2.    [lx⋅uy,  lx⋅ly]
;;            ╰─────l─────╯
;;
;;     ✗     (−)(−)   (+)(−)
;;
;;               ╭──u──╮
;;     ✓     (−)(−)   (+)(+)     3.    [lx⋅uy,  ux⋅ly]
;;            ╰─────l─────╯
;;
;;            ╭───u────╮
;;     ✓     (−)(+)   (−)(−)     4.    [ux⋅ly,  lx⋅ly]
;;               ╰──l──╯
;;
;;            ╭────────┬──╮
;;     ✓     (−)(+)   (−)(+)     5.    4 multiplications, choose min, max
;;               ╰─────┴──╯
;;
;;     ✗     (−)(+)   (+)(−)
;;
;;               ╭───u────╮
;;     ✓     (−)(+)   (+)(+)     6.    [lx⋅uy,  ux⋅uy]
;;            ╰─────l─────╯
;;
;;     ✗     (+)(−)   (−)(−)
;;     ✗     (+)(−)   (−)(+)
;;     ✗     (+)(−)   (+)(−)
;;     ✗     (+)(−)   (+)(+)
;;
;;            ╭─────u─────╮
;;     ✓     (+)(+)   (−)(−)     7.    [ux⋅ly,  lx⋅uy]
;;               ╰──l──╯
;;
;;               ╭────u───╮
;;     ✓     (+)(+)   (−)(+)     8.    [ux⋅ly,  ux⋅uy]
;;               ╰──l──╯
;;
;;     ✗     (+)(+)   (+)(−)
;;
;;               ╭────u───╮
;;     ✓     (+)(+)   (+)(+)     9.    [lx⋅ly,  ux⋅uy]
;;            ╰───l────╯


(define (make-interval a b) 
  (if (<= a b) 
      (cons a b)
      (printf "Lower bound must be at most equal to upper bound.\n")))

(define (mul-interval x y)
  (let ((lx (lower-bound x)) (ux (upper-bound x))
	(ly (lower-bound y)) (uy (upper-bound y))
	(-? negative?) (+? positive?) (mint make-interval))
    (cond ((and (-? lx) (-? ux))                                       ; cases:
	   (cond ((and (-? ly) (-? uy)) (mint (* ux uy) (* lx ly)))      ; 1.
		 ((and (-? ly) (+? uy)) (mint (* lx uy) (* lx ly)))      ; 2. 
		 ((and (+? ly) (+? uy)) (mint (* lx uy) (* ux ly)))))    ; 3.
	  ((and (-? lx) (+? ux))
	   (cond ((and (-? ly) (-? uy)) (mint (* ux ly) (* lx ly)))      ; 4.
		 ((and (-? ly) (+? uy))                                  ; 5.
		  (let ((p1 (* lx ly)) (p2 (* lx uy))
			(p3 (* ux ly)) (p4 (* ux uy)))
		    (mint (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
		 ((and (+? ly) (+? uy)) (mint (* lx uy) (* ux uy)))))    ; 6.
	  ((and (+? lx) (+? ux))
	   (cond ((and (-? ly) (-? uy)) (mint (* ux ly) (* lx uy)))      ; 7.
		 ((and (-? ly) (+? uy)) (mint (* ux ly) (* ux uy)))      ; 8.
		 ((and (+? ly) (+? uy)) (mint (* lx ly) (* ux uy)))))))) ; 9.

(define i5 (make-interval 4.4 4.9))
(define i6 (make-interval 7.2 7.6))

(define i7 (make-interval 3.4 3.9))
(define i8 (make-interval 9.2 9.6))

(mul-interval i5 i6)
(mul-interval i7 i8)
