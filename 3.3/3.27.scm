
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (same-key? key1 key2)
  (define tolerance 0.01)
  (if (and (number? key1) (number? key2))
      (< (abs (- key1 key2)) tolerance)
      (equal? key1 key2)))

(define (lt key1 key2)
  (compare < string<? key1 key2))

(define (gt key1 key2)
  (compare > string>? key1 key2))

(define (compare numcomp strcomp key1 key2)
  (if (and (number? key1) (number? key2))
      (numcomp key1 key2)
      (strcomp (tostring key1) (tostring key2))))

(define (tostring val)
  (cond ((string? val) val)
	((symbol? val) (symbol->string val))
	((number? val) (number->string val))
	(else (error "tostring: argument must be an atom, given" val))))

(define (make-table)
  (let ((local-table (list '*table* (list 0))))
    (define tablekey car)
    (define value cdr)
    (define (lookup key)
      (assoc #f key 'void (cadr local-table)))
    (define (insert! key val)
      (assoc #t key val (cadr local-table))
      'ok)
    ;; For both reading and writing the value under a key 
    ;; in associative map organized as a binary tree:
    (define (assoc write? key val tree)
      (cond ((and (number? (car tree))
		  (zero? (car tree)))
	     (if write? 
		 (begin (set-car! tree (cons key val))
			(set-cdr! tree (list (list 0) (list 0))))
		 false))
	    ((same-key? key (tablekey (entry tree)))
	     (if write? 
		 (set-cdr! (entry tree) val)
		 (value (entry tree))))
	    ((lt key (tablekey (entry tree)))
	     (assoc write? key val (left-branch tree)))
	    ((gt key (tablekey (entry tree)))
	     (assoc write? key val (right-branch tree)))
	    (else (error "This location should be unreachable."))))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert!)
	    ((eq? m 'table) local-table)
	    ((eq? m 'tree) (cadr local-table))
	    (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define (lookup key table)
  ((table 'lookup) key))

(define (insert! key val table)
  ((table 'insert!) key val))

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(define memo-fib
  (memoize (lambda (n)
	     (cond ((= n 0) 0)
		   ((= n 1) 1)
		   (else (+ (memo-fib (- n 1))
			    (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
	(or previously-computed-result
	    (let ((result (f x)))
	      (insert! x result table)
	      result))))))

(memo-fib 200) ; 280571172992510140037611932413038677189525
;; Takes less than a second, while (memo-fib 40) without memoization
;; takes several minutes.

;; The order of growth is O(n) because the algorithm only performs
;; a constant number of operations per n. First, it steps down the 
;; leftmost branch until depth n. There, reaching the base cases n = 1 
;; or n = 0, it tabulates the values, and steps back up. Then it evaluates
;; the local right branch memo-fib expression. But its argument is one
;; less than left branch argument, which means the same result is already
;; computed and tabulated in the previous step down the tree. It simply 
;; gets the value from table, adds it to the left branch value, and
;; returns the sum as the next result up the tree. So the algorithm
;; performs at most two lookups, one recursive call to memo-fib and
;; one insert! at each node of the leftmost branch. Number of nodes is n.


(define memo-fib (memoize fib))

(memo-fib 30)

;; No, it won't work, because fib will call itself instead of memo-fib.
;; After calling (f x), which invokes (fib x), it inserts just one
;; result into the table. But this is already the final result after
;; the whole tree recursive process is finished.
