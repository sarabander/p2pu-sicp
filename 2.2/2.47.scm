
;; 1st implementation
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)

(define edge1-frame cadr)

(define edge2-frame caddr)

;; Quick test
(define frame1 (make-frame 'o 'e1 'e2))
(origin-frame frame1)
(edge1-frame frame1)
(edge2-frame frame1)

;; 2nd implementation
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame car)

(define edge1-frame cadr)

(define edge2-frame cddr)

;; Quick test
(define frame1 (make-frame 10 20 30))
(origin-frame frame1)
(edge1-frame frame1)
(edge2-frame frame1)
