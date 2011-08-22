(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame) (start-segment segment))
;        ((frame-coord-map frame) (end-segment segment))))
;     segment-list)))

;; a.
(define outlinelist (list (make-segment (make-vect 0 0)       (make-vect 0 0.99))
                          (make-segment (make-vect 0 0.99)    (make-vect 0.99 0.99))
                          (make-segment (make-vect 0.99 0.99) (make-vect 0.99 0))
                          (make-segment (make-vect 0.99 0)    (make-vect 0 0))))

(define outline (segments->painter outlinelist))
(paint outline)

;; b.
(define xlist (list (make-segment (make-vect 0 0) (make-vect 1 1))
                    (make-segment (make-vect 1 0) (make-vect 0 1))))

(define x (segments->painter xlist))
(paint x)

;; c.
(define diamondlist (list (make-segment (make-vect 0.5 0)    (make-vect 0 0.5))
                          (make-segment (make-vect 0 0.5)    (make-vect 0.5 0.99))
                          (make-segment (make-vect 0.5 0.99) (make-vect 0.99 0.5))
                          (make-segment (make-vect 0.99 0.5) (make-vect 0.5 0))))

(define diamond (segments->painter diamondlist))
(paint diamond)

;; d. 
(define wavelist (list (make-segment (make-vect 0.2 0)    (make-vect 0.4 0.5))
                       (make-segment (make-vect 0.4 0.5)  (make-vect 0.2 0.5))
                       (make-segment (make-vect 0.2 0.5)  (make-vect 0 0.7))
                       (make-segment (make-vect 0 0.9)    (make-vect 0.2 0.7))
                       (make-segment (make-vect 0.2 0.7)  (make-vect 0.4 0.7))
                       (make-segment (make-vect 0.4 0.7)  (make-vect 0.35 0.8))
                       (make-segment (make-vect 0.35 0.8) (make-vect 0.4 1))
                       (make-segment (make-vect 0.6 0.99) (make-vect 0.65 0.8))
                       (make-segment (make-vect 0.65 0.8) (make-vect 0.6 0.7))
                       (make-segment (make-vect 0.6 0.7)  (make-vect 0.7 0.7))
                       (make-segment (make-vect 0.7 0.7)  (make-vect 0.99 0.5))
                       (make-segment (make-vect 0.99 0.3) (make-vect 0.6 0.5))
                       (make-segment (make-vect 0.6 0.5)  (make-vect 0.8 0))
                       (make-segment (make-vect 0.6 0)    (make-vect 0.5 0.3))
                       (make-segment (make-vect 0.5 0.3)  (make-vect 0.4 0))))

(define wave (segments->painter wavelist))
(paint wave)
