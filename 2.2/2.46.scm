
(define make-vect cons)

(define xcor-vect car)

(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;; Tests
(scale-vect 3 (add-vect (make-vect 3 4)
                        (make-vect 2 -6)))

(sub-vect (make-vect 3 4)
          (make-vect 2 -6))
