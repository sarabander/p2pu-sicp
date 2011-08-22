(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;; Original splits
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

;; More general split
(define (split f1 f2)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller ((split f1 f2) painter (- n 1))))
        (f1 painter (f2 smaller smaller))))))
    
(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split einstein 4))
(paint (up-split einstein 4))
