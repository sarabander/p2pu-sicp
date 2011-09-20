
;; Look in 2.68 and 2.71 for dependencies

(define huffman1
  (generate-huffman-tree alphabet1))

(define huffman2
  (generate-huffman-tree alphabet2))

(define counter 0)

(define (encode-symbol symbol tree)
  (set! counter (add1 counter))
      (cond ((leaf? tree) empty)
	    ((contains? symbol (left-branch  tree)) 
	     (cons 0 (encode-symbol symbol (left-branch  tree))))
	    ((contains? symbol (right-branch tree)) 
	     (cons 1 (encode-symbol symbol (right-branch tree))))
	    (else (error "Unrecognized symbol:" symbol))))

(define (contains? symbol tree)
  (define (search symb lst)
    (set! counter (add1 counter))
    (cond ((empty? lst) false)
	  ((eq? symb (car lst)) true)
	  (else (search symb (cdr lst)))))
  (search symbol (symbols tree)))

(set! counter 0)

(encode-symbol 'A huffman1) ; counter = 23
(encode-symbol 'A huffman2) ; counter = 73

(encode-symbol 'E huffman1) ; counter = 3
(encode-symbol 'J huffman2) ; counter = 3

;; We can encode the most frequent symbol in constant time.
;; Encoding the least frequent symbol has order of O(n²) growth, 
;; because both 'encode-symbol' and 'contains?' has O(n) growth.

;; In worst case scenario, 'encode-symbol' calls 'contains?' twice.
;; Left branch takes constant time with this particular tree.
;; Right branch takes at most O(n) time.
