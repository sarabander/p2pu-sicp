(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;; a.
;; Make legs larger, add fingers
(define wavelist (list (make-segment (make-vect 0.1 0)    (make-vect 0.4 0.55))
                       (make-segment (make-vect 0.4 0.55) (make-vect 0.2 0.5))
                       (make-segment (make-vect 0.2 0.5)  (make-vect 0 0.7))
                       (make-segment (make-vect 0 0.9)    (make-vect 0.2 0.7))
                       (make-segment (make-vect 0.2 0.7)  (make-vect 0.4 0.7))
                       (make-segment (make-vect 0.4 0.7)  (make-vect 0.35 0.8))
                       (make-segment (make-vect 0.35 0.8) (make-vect 0.4 1))
                       (make-segment (make-vect 0.6 0.99) (make-vect 0.65 0.8))
                       (make-segment (make-vect 0.65 0.8) (make-vect 0.6 0.7))
                       (make-segment (make-vect 0.6 0.7)  (make-vect 0.7 0.7))
                       (make-segment (make-vect 0.7 0.7)  (make-vect 0.99 0.5))
                       (make-segment (make-vect 0.99 0.3) (make-vect 0.6 0.55))
                       (make-segment (make-vect 0.6 0.55) (make-vect 0.9 0))
                       (make-segment (make-vect 0.6 0)    (make-vect 0.5 0.3))
                       (make-segment (make-vect 0.5 0.3)  (make-vect 0.4 0))
                       (make-segment (make-vect 1 0.35)   (make-vect 0.9 0.40))
                       (make-segment (make-vect 1 0.41)   (make-vect 0.9 0.46))
                       (make-segment (make-vect 1 0.46)   (make-vect 0.9 0.51))
                       (make-segment (make-vect 0 0.75)   (make-vect 0.1 0.65))
                       (make-segment (make-vect 0 0.80)   (make-vect 0.1 0.70))
                       (make-segment (make-vect 0 0.85)   (make-vect 0.1 0.75))))

(define wave (segments->painter wavelist))
(paint wave)

;; b.
;; Make changes in corner-split
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; Original
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint (corner-split wave 4))

;; Changed
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint (corner-split wave 4))

;; c.
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; Original
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(paint (square-limit einstein 4))

;; Change position of squares
(define (square-of-four br bl tr tl)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(paint (square-limit einstein 4))
