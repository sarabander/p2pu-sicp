
;; Uses these procedures from the book
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))
;; ----------------------

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (successive-merge 
       (adjoin-set (make-code-tree (car  leaf-set)
				   (cadr leaf-set))
		   (cddr leaf-set)))))

(generate-huffman-tree '()) ; invalid input
(generate-huffman-tree '((A 2))) ; '(leaf A 2)
(generate-huffman-tree '((A 3) (B 1))) ; '((leaf B 1) (leaf A 3) (B A) 4)
(generate-huffman-tree '((B 2) (D 1) (A 4) (C 1)))
;; '((leaf A 4)
;;   ((leaf B 2) ((leaf C 1) (leaf D 1) (C D) 2) (B C D) 4)
;;   (A B C D)
;;   8)

(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
;; '((leaf A 4)
;;   ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4)
;;   (A B D C)
;;   8)

(generate-huffman-tree 
 '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))

(generate-huffman-tree 
 (reverse '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1))))
