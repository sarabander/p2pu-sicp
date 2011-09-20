
(define (encode message tree)
  (if (null? message)
      '()
       (append (encode-symbol (car message) tree)
	       (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
      (cond ((leaf? tree) empty)
	    ((contains? symbol (left-branch  tree)) 
	     (cons 0 (encode-symbol symbol (left-branch  tree))))
	    ((contains? symbol (right-branch tree)) 
	     (cons 1 (encode-symbol symbol (right-branch tree))))
	    (else (error "Unrecognized symbol:" symbol))))

;; Naïve version (2n steps)
(define (contains? symbol tree)
  (if (leaf? tree)
      (if (eq? symbol (symbol-leaf tree))
	  true
	  false)
      (or (contains? symbol (left-branch  tree))
	  (contains? symbol (right-branch tree)))))

;; Better version (n steps)
(define (contains? symbol tree)
  (define (search symb lst)
    (cond ((empty? lst) false)
	  ((eq? symb (car lst)) true)
	  (else (search symb (cdr lst)))))
  (search symbol (symbols tree)))

;; Unit tests
(contains? 'A sample-tree) ; true
(contains? 'B sample-tree) ; true
(contains? 'C sample-tree) ; true
(contains? 'D sample-tree) ; true

(contains? 'E sample-tree) ; false
(contains? 'R sample-tree) ; false
(contains? 'a sample-tree) ; false (symbols are case-sensitive)

(encode-symbol 'A sample-tree) ; '(0)
(encode-symbol 'B sample-tree) ; '(1 0)
(encode-symbol 'C sample-tree) ; '(1 1 1)
(encode-symbol 'D sample-tree) ; '(1 1 0)

(encode-symbol 'F sample-tree) ; => Unrecognized symbol: F
(encode-symbol 'S sample-tree) ; => Unrecognized symbol: S

;; Encoding the result of 2.67

(encode '(A D A B B C A) sample-tree) ; '(0 1 1 0 0 1 0 1 0 1 1 1 0)
(decode '(0 1 1 0 0 1 0 1 0 1 1 1 0) sample-tree) ; '(A D A B B C A)

;; correct!
