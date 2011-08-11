
;; From the book
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))
;; -------------

;; a.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;; Tests
(define mob1
  (make-mobile (make-branch 3 6)
	       (make-branch 2 9)))

(define mob2
  (make-mobile (make-branch 6 4)
	       (make-branch 5 mob1)))

(define mob3
  (make-mobile (make-branch 15 3)
	       (make-branch 3 mob1)))

(branch-structure (right-branch mob1))
(branch-structure (right-branch mob2))

;; b.

(define (total-weight mobile)
  (cond ((not (pair? mobile)) mobile)
	(else (+ (total-weight (branch-structure (left-branch mobile)))
		 (total-weight (branch-structure (right-branch mobile)))))))

(total-weight mob1) ; 15
(total-weight mob2) ; 19
(total-weight mob3) ; 18

;; c.

(define (balanced? mobile)
  (let ((left  (left-branch  mobile))
	(right (right-branch mobile))
	(b-struct branch-structure)
	(b-length branch-length))
    (= (* (total-weight (b-struct left))
	  (b-length left))
       (* (total-weight (b-struct right))
	  (b-length right)))))

(define (balanced? mobile)
  (if (number? mobile)
      true
      (let ((left  (left-branch  mobile))
	    (right (right-branch mobile))
	    (b-struct branch-structure)
	    (b-length branch-length))
	(and (= (* (total-weight (b-struct left))
		   (b-length left))
		(* (total-weight (b-struct right))
		   (b-length right)))
	     (balanced? (b-struct left))
	     (balanced? (b-struct right))))))

(balanced? mob1) ; true
(balanced? mob2) ; false
(balanced? mob3) ; true

;; d.

;; Redefinitions from the book
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))
;; -------------

;; Only the selectors must be changed:

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))
