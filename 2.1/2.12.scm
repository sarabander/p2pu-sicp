;; From the book
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
;; ----------------------

(define (make-center-percent c p)
  (make-center-width c (* c p)))

(define (percent i)
  (/ (width i) (center i)))

;; Tests
(define 5R10% (make-center-percent 5.0 0.1))

(percent 5R10%) ; => 0.1
(width 5R10%)   ; => 0.5
(center 5R10%)  ; => 5.0
