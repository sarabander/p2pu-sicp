
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

;; 1.
(set! x (* x x))
(set! x (* x x x))
;; or
(set! x (* x x x))
(set! x (* x x))
;; Same result:
; x = 1000000

;; 2. 
;; First lambda accesses x twice, * evaluates to 100.
;; Second lambda sets x to 1000.
;; First lambda sets x to 100.
;; x = 100

;; 3.
;; Second λ accesses x three times, * evaluates to 1000.
;; First λ sets x to 100.
;; Second lambda sets x to 1000.
;; x = 1000

;; 4.
;; First λ accesses x inside * and gets 10,
;; second lambda sets x to 1000, first λ accesses second x, gets 1000,
;; and sets x to (* 10 1000). Same result if second λ accesses x inside
;; * twice and gets 10, then first λ sets x to 100, second λ accesses x
;; third time, and sets x to (* 10 10 100):
;; x = 10000

;; 5.
;; Second λ accesses x inside * and gets 10,
;; first λ sets x to 100, second λ accesses x second and third time,
;; gets 100, and sets x to (* 10 100 100):
;; x = 100000

(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

;; After this, only the first possibility remains:
;; x = 1000000
