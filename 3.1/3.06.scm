
;; Original rand introduced in the text 
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

;; New version listening to two messages
(define rand
  (let ((x random-init))
    (define (dispatch message)
      (cond ((eq? message 'generate)
	     (set! x (rand-update x))
	     x)
	    ((eq? message 'reset)
	     (lambda (new)
	       (set! x new)
	       (set! x (rand-update x))
	       x))
	    (else (error "Message not understood:" message))))
    dispatch))
