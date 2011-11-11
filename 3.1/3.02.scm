
(define (make-monitored f)
  (let ((counter 0))
    (define (dispatch message)
      (cond ((eq? message 'how-many-calls?)
	     counter)
	    ((eq? message 'reset-count)
	     (set! counter 0)
	     counter)
	    (else (set! counter (add1 counter))
		  (f message))))
    dispatch))

(define s (make-monitored sqrt))

(s 'how-many-calls?) ; 0
(s 'how-many-calls?) ; 0

(s 169) ; 13

(s 'how-many-calls?) ; 1
(s 'how-many-calls?) ; 1

(s 49) ; 7

(s 'how-many-calls?) ; 2
(s 'reset-count) ; 0
(s 'how-many-calls?) ; 0

(s 10) ; 3.1622776601683795

(s 'how-many-calls?) ; 1

(s 4)   ; 2
(s 25)  ; 5
(s 625) ; 25

(s 'how-many-calls?) ; 4
