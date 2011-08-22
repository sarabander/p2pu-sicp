(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;; Modified to use the transform-painter from hend.scm (in sicp.plt)
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

(paint (beside einstein einstein))

(define flip-horiz
  (transform-painter (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(paint (flip-horiz einstein))

(define rotate180
  (transform-painter (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))

(paint (rotate180 einstein))

(define rotate270
  (transform-painter (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(paint (rotate270 einstein))
