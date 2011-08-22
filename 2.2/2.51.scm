(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;; First version
(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-top
           (transform-painter split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0)))
          (paint-bottom
           (transform-painter (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point)))
      (superpose 
       (paint-top painter2)
       (paint-bottom painter1)))))

(paint (below gray einstein))

;; Second version
(define (below painter1 painter2)
  (rotate270 (rotate180 (beside (rotate270 painter1)
                                (rotate270 painter2)))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (superpose 
       (paint-left painter1)
       (paint-right painter2)))))

(paint (below gray einstein))
