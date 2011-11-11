
;; Works in MIT-Scheme

(define rand
    (lambda ()
      (random 1000)
      ))

(rand)

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1)
		 (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1)
		 trials-passed))))
  (iter trials 0))

(estimate-pi 1000)    ;Value: 3.108349360801046
(estimate-pi 10000)   ;Value: 3.151267458316272
(estimate-pi 1000000) ;Value: 3.1410504473251293

;; -------------------------------------

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(random-in-range 0.1 0.9)

(define (make-point x y)
  (list x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cadr point))

(define (point-in-region? point region)
  (region point))

(define disk1 (lambda (point)
		(let ((x (x-point point))
		      (y (y-point point)))
		  (<= (+ (square (- x 5)) (square (- y 7)))
		      (square 3)))))

(point-in-region? (make-point 5 7) disk1) ; true
(point-in-region? (make-point 2 4) disk1) ; false
(point-in-region? (make-point 8 10) disk1) ; false

(define disk2 (lambda (point)
		(let ((x (x-point point))
		      (y (y-point point))
		      (x-center 0)
		      (y-center 0)
		      (radius 1))
		  (<= (+ (square (- x x-center)) 
			 (square (- y y-center)))
		      (square radius)))))

(point-in-region? (make-point 0 0) disk2) ; true
(point-in-region? (make-point 0 1) disk2) ; true
(point-in-region? (make-point 0 1.1) disk2) ; false
(point-in-region? (make-point 1 1) disk2) ; false

;; Rectangles

;; lower-left and upper-right are points (x, y)
(define (make-rectangle lower-left upper-right)
  (cons lower-left upper-right))
(define (lower-left rectangle)
  (car rectangle))
(define (upper-right rectangle)
  (cdr rectangle))
(define (x-lower rectangle)
  (x-point (lower-left rectangle)))
(define (x-upper rectangle)
  (x-point (upper-right rectangle)))
(define (y-lower rectangle)
  (y-point (lower-left rectangle)))
(define (y-upper rectangle)
  (y-point (upper-right rectangle)))
(define (width rectangle)
  (- (x-upper rectangle)
     (x-lower rectangle)))
(define (height rectangle)
  (- (y-upper rectangle)
     (y-lower rectangle)))
(define (area rectangle)
  (* (width  rectangle)
     (height rectangle)))
    
(define rect1 (make-rectangle (make-point 2 4) 
			      (make-point 8 10)))

(x-lower rect1) ; 2
(x-upper rect1) ; 8
(y-lower rect1) ; 4
(y-upper rect1) ; 10

(define rect2 (make-rectangle (make-point -1.0 -1.0) 
			      (make-point 1.0 1.0)))

(x-lower rect2) ; -1
(x-upper rect2) ;  1
(y-lower rect2) ; -1
(y-upper rect2) ;  1

(define (random-point-in-rectangle rectangle)
  (make-point (random-in-range (x-lower rectangle)
			       (x-upper rectangle))
	      (random-in-range (y-lower rectangle)
			       (y-upper rectangle))))

(random-point-in-rectangle rect1) ; (4 7)

(random-point-in-rectangle rect2) 
;Value 48: (.6318383337433833 -.9600126255480919)

(define (disk-hit-test disk rectangle)
  (point-in-region?
   (random-point-in-rectangle rectangle)
   disk))

;; (define (disk2-testshoot)
;;   (disk-hit-test disk2 rect2))

(define (estimate-integral region-predicate rectangle trials)
  (let ((test (lambda () (disk-hit-test region-predicate
			     rectangle))))
    (let ((hitfraction (monte-carlo trials test)))
      (* hitfraction (area rectangle)))))

(define (pi-estimation trials)
  (estimate-integral disk2 rect2 trials))

(pi-estimation 1000) ;Value: 3.184
(pi-estimation 1000) ;Value: 3.068

(pi-estimation 100000) ;Value: 3.13788
(pi-estimation 100000) ;Value: 3.14764

(pi-estimation 1000000) ;Value: 3.140216
(pi-estimation 1000000) ;Value: 3.142132

(pi-estimation 10000000) ;Value: 3.1425508
(pi-estimation 10000000) ;Value: 3.1423884
(pi-estimation 10000000) ;Value: 3.1411892
