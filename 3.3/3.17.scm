
;; First version can't handle cyclic lists
(define (count-pairs x)
  (define pairs (make-hash-table))
  (define (count-pairs-1 x)
    (if (or (not (pair? x))
	    (hash-table/get pairs x #f))
	false
	(begin 
	  (hash-table/put! pairs x #t)
	  (count-pairs-1 (car x))
	  (count-pairs-1 (cdr x)))))
  (count-pairs-1 x)
  (hash-table/count pairs))

(count-pairs p) ; 3
(count-pairs q) ; 3
(count-pairs s) ;Aborting!: maximum recursion depth exceeded
(count-pairs z) ;Aborting!: maximum recursion depth exceeded

;; Second version processes cyclic lists correctly
(define (count-pairs lst)
  (define tracker '())
  (define (count-pairs-1 x)
    (if (or (not (pair? x))
	    (> (length (filter (lambda (item) 
				 (eq? item x)) 
			       tracker)) 
	       0))
	false
	(begin 
	  (set! tracker (cons x tracker))
	  (count-pairs-1 (car x))
	  (count-pairs-1 (cdr x)))))
  (count-pairs-1 lst)
  (length tracker))

(count-pairs p) ; 3
(count-pairs q) ; 3
(count-pairs r) ; 3
(count-pairs s) ; 3  [cyclic]
(count-pairs z) ; 4  [cyclic]
(count-pairs '(1 2 3 4 5)) ; 5
