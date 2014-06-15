
; 1.

(define (make-semaphore n) ; n >= 0
  (let ((mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'wait)
             (cond ((> n 1) (set! n (- n 1)))
                   ((= n 1) (begin (set! n 0)
                                   (mutex 'acquire)))
                   (else (the-semaphore 'wait)))) ; n = 0
            ((eq? m 'signal)
             (if (zero? n)
                 (begin (set! n 1)
                        (mutex 'release))
                 (set! n (+ n 1))))))
    the-semaphore))

; 2.

(define (make-semaphore n) ; n >= 0
  (let ((cell (list false)))
    (define (the-semaphore m)
      (cond ((eq? m 'wait)
             (cond ((> n 1) (set! n (- n 1)))
                   ((= n 1) (begin (set! n 0)
                                   (test-and-set! cell)))
                   (else (the-semaphore 'wait)))) ; n = 0
            ((eq? m 'signal)
             (if (zero? n)
                 (begin (set! n 1)
                        (clear! cell))
                 (set! n (+ n 1))))))
    the-semaphore))

