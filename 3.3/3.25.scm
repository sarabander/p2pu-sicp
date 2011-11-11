
(define (same-key? key1 key2)
  (define tolerance 0.01)
  (if (and (number? key1) (number? key2))
      (< (abs (- key1 key2)) tolerance)
      (equal? key1 key2)))

(define (make-table comparator)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (lookup-1 keys table)
	(let ((subtable (assoc (car keys) (cdr table))))
	  (if subtable
	      (if (null? (cdr keys))
		  (cadr subtable)
		  (lookup-1 (cdr keys) subtable))
	      false)))
      (lookup-1 keys local-table))
    (define (insert! keys value)
      (define (newtable keys)
	(if (null? keys)
	    value
	    (list (car keys) (newtable (cdr keys)))))
      (define (insert!-1 keys table)
	(let ((subtable (assoc (car keys) (cdr table))))
	  (if subtable
	      (if (null? (cdr keys))
		  (set-cdr! subtable (list value))
		  (insert!-1 (cdr keys) subtable))
	      (set-cdr! table 
			(cons (newtable keys)
			      (cdr table)))))
	'ok)
      (insert!-1 keys local-table))
    (define (assoc key records)
      (cond ((null? records) false)
	    ((comparator key (caar records)) (car records))
	    (else (assoc key (cdr records)))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define tbl3 (make-table same-key?))

((tbl3 'insert-proc!) '(1 2 3) "alligator")
((tbl3 'insert-proc!) '(1 2 5) "cayman")

((tbl3 'lookup-proc) '(0.999 2 3.0))
((tbl3 'lookup-proc) '(1 2.01 4.99))

((tbl3 'lookup-proc) '(1))

((tbl3 'insert-proc!) '(1 2 4 7 10) "ant")
