
(define callcount 0)

; Side-effected version:
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (begin (set! callcount (+ callcount 1)) ; *
              (apply proc (map stream-car argstreams)))
       (apply stream-map
              (cons proc 
                    (map stream-cdr 
                         argstreams))))))

;     1 1 2 3 5  8 13 21 ... = (stream-cdr fibs)
;  +  0 1 1 2 3  5  8 13 ... = fibs
; --------------------------
; 0 1 1 2 3 5 8 13 21 34 ... = fibs

; Needs to be redefined before each call-counting experiment below
(define fibs 
  (cons-stream 
   0 (cons-stream
      1 (add-streams 
         (stream-cdr fibs) fibs))))

(set! callcount 0)
(stream-ref fibs 5) ; 5
callcount ; 4

(set! callcount 0)
(stream-ref fibs 6) ; 8
callcount ; 5

(set! callcount 0)
(stream-ref fibs 7) ; 13
callcount ; 6

(set! callcount 0)
(stream-ref fibs 10) ; 55
callcount ; 9

; We perform n-1 additions to obtain the n-th Fibonacci number.

;; Book version of delay and force, no memoization

(define-syntax delay
  (syntax-rules ()
    ((delay exp)
     (lambda () exp))))

(define (force delayed-object)
  (delayed-object))

; Measurement with non-memoized version
(set! callcount 0)
(stream-ref fibs 5) ; 5
callcount ; 14

(set! callcount 0)
(stream-ref fibs 6) ; 8
callcount ; 26

(set! callcount 0)
(stream-ref fibs 7) ; 13
callcount ; 46

(set! callcount 0)
(stream-ref fibs 10) ; 55
callcount ; 221

;; Number of additions grows as 2^n. Base could be 1.8 instead of 2,
;; because addition operations do not quite double when n increments
;; by one.

;; delay implemented with memoization

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((delay exp)
     (memo-proc (lambda () exp)))))

