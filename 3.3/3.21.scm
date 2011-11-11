
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	   (set-cdr! (rear-ptr queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE! called with an empty queue" queue))
	(else
	 (set-front-ptr! queue (cdr (front-ptr queue)))
	 queue)))

;; ---------------------------------------------------------

(define q1 (make-queue))
(insert-queue! q1 'a) ; ((a) a)

(insert-queue! q1 'b) ; ((a b) b)

(delete-queue! q1) ; ((b) b)

(delete-queue! q1) ; (() b)

;; The Scheme printer just prints q1 as ordinary pair. Its car is the list 
;; consisting of the entire queue, and its cdr is the last item of the queue.

;; Assuming the last item of list pointed by front-ptr and the item 
;; pointed by rear-ptr coincide.
(define (print-queue queue)
  (front-ptr queue))

(print-queue q1) ; (a b) 
;; after (insert-queue! q1 'b)
