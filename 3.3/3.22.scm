
(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    ;; definitions of internal procedures
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT called with an empty queue" front-ptr)
	  (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
	       (set-front-ptr! new-pair)
	       (set-rear-ptr!  new-pair)
	       front-ptr)
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set-rear-ptr! new-pair)
	       front-ptr))))

    (define (delete-queue!)
      (cond ((empty-queue?)
	     (error "DELETE! called with an empty queue" front-ptr))
	    (else
	     (set-front-ptr! (cdr front-ptr))
	     front-ptr)))

    (define (dispatch m) 
      (cond ((eq? m 'front) (front-queue))
	    ((eq? m 'insert) insert-queue!)
	    ((eq? m 'delete) (delete-queue!))
	    ((eq? m 'print) front-ptr)
	    (else (error "Unrecognized message" m))))
    dispatch))


;; Queue operations
(define (front-queue queue)
  (queue 'front))

(define (insert-queue! queue item)
  ((queue 'insert) item))

(define (delete-queue! queue)
  (queue 'delete))

(define (print-queue queue)
  (queue 'print))

;; Tests
(define q2 (make-queue)) ; q2

q2 ; #[compound-procedure 32 dispatch]

(print-queue q2) ; ()

(insert-queue! q2 'k) ; (k)
(insert-queue! q2 'l) ; (k l)
(insert-queue! q2 'm) ; (k l m)

(front-queue q2) ; k

(delete-queue! q2) ; (l m)

(print-queue q2) ; (l m)
