
;; Avoids printing a pair more than once.
;; Modeled after count-pairs in 3.17.
(define (avoid-cycles lst)
  (define tracker '()) ; holds a list of encountered pairs
  (define (avoid-cycles-1 x)
    (cond ((not (pair? x)) x)
	  ((> (length (filter (lambda (item) 
				(eq? item x)) 
			      tracker)) 
	      0) 
	   '())
	  (else (begin 
		  (set! tracker (cons x tracker))
		  (cons (avoid-cycles-1 (car x))
			(avoid-cycles-1 (cdr x)))))))
  (avoid-cycles-1 lst))

(avoid-cycles '(1 2 3)) ; (1 2 3)

;; s and z are cyclic
(avoid-cycles s) ; (a () c)
(avoid-cycles z) ; (a b c)


(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cddr deque))
(define (before-rear-ptr deque) (cadr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! (cdr deque) item))
(define (set-before-rear-ptr! deque item) (set-car! (cdr deque) item))
(define (empty-deque? deque) (null? (front-ptr deque)))
(define (make-deque) (cons '() (cons '() '())))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   (avoid-cycles deque))
	  (else
	   (set-cdr! new-pair (front-ptr deque))
	   (set-front-ptr! deque new-pair)
	   (avoid-cycles deque)))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   (avoid-cycles deque))
	  (else
	   (set-before-rear-ptr! deque (rear-ptr deque))
	   (set-cdr! (rear-ptr deque) new-pair)
	   (set-rear-ptr! deque new-pair)
	   (avoid-cycles deque)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE! called with an empty deque" deque))
	(else
	 (set-front-ptr! deque (cdr (front-ptr deque)))
	 (avoid-cycles deque))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE! called with an empty deque" deque))
	(else
	 (set-rear-ptr! deque (before-rear-ptr deque))
	 (set-cdr! (rear-ptr deque) '())
	 (avoid-cycles deque))))

;; Assuming the last item of list pointed by front-ptr and the item 
;; pointed by rear-ptr coincide.
(define (print-deque deque)
  (avoid-cycles (front-ptr deque)))


;; Tests
(define deq1 (make-deque))
deq1 ; (() ())
(print-deque deq1) ; ()

(front-insert-deque! deq1 'g)
(front-insert-deque! deq1 'a)
(print-deque deq1) ; (a g)

(rear-insert-deque! deq1 'r)
(rear-insert-deque! deq1 'i)
(print-deque deq1) ; (a g r i)

(front-delete-deque! deq1)
(print-deque deq1) ; (g r i)

(rear-delete-deque! deq1)
(print-deque deq1) ; (g r)
