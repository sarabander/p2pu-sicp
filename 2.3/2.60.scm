
;; Same as in book.
;; Grows as before: O(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

;; No reason to check membership.
;; This now grows as O(1) instead of O(n).
(define (adjoin-set x set)
      (cons x set))

;; Unchanged from book.
;; Grows as before: O(n²).
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

;; No need to check membership.
;; Grows as O(1) instead of O(n²).
(define (union-set set1 set2)
  (append set1 set2))

(union-set '(1 5) '(1 2 4)) ; '(1 5 1 2 4)

;; Sure, I would use this for example to find unions of book collections.
;; I don't mind having duplicate books in the library.
