
;; If you haven't already encountered this problem, 
;; then now is an excellent opportunity for deep thought.
;; Resist the temptation to search the literature for a while.
;; You can independently come up with an elegant and simple
;; solution if you just relax and close your eyes. Use your
;; geometric intuition. After the sudden jolt of revelation, 
;; you will be rewarded with joy and happiness by the spirits 
;; living in the computer. 


;; SPOILER ALERT! 




















;; Cyclic? probes for a loop in list structure. 
;; --------------------------------------------
;; It puts two pointers running down the list,
;; one starts ahead, and is twice as fast as the other.
;; Slow pointer advances if switch is turned on.
;; If there is a cycle, fast one catches slow one, 
;; and they finally point to the same cell.

(define (cyclic? lst)
  (define (iter switch slow-pointer fast-pointer)
    (cond ((null? fast-pointer) #f)
	  ((eq? slow-pointer fast-pointer) #t)
	  (else (iter (not switch) 
		      (if switch (cdr slow-pointer) slow-pointer)
		      (cdr fast-pointer)))))
  (iter #t lst (cdr lst)))


;; Tests
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c 'd)))
(define w (append '(g h j) z))

(count-pairs w) ; 7

;; p, q, r and s are defined in 3.16

(cyclic? z) ; true
(cyclic? w) ; true
(cyclic? p) ; false
(cyclic? q) ; false
(cyclic? r) ; false
(cyclic? s) ; false (cycles starting from car of a pair are not detected)
(cyclic? '(1 2 3 1 2 3 1 2 3)) ; false

(define (make-long-list length)
  (define (iter counter lst)
    (if (= counter 0)
	lst
	(iter (- counter 1) (cons (random 10) lst))))
  (iter length '()))

(define long1 (make-long-list 1000000))
(define long1cyc (make-cycle long1)) ; changes long1 in place and makes alias
(define long2 (make-long-list 200000))
(define long3 (append long2 long1))

(cyclic? long1) ; true
(cyclic? long1cyc) ; true
(cyclic? long2) ; false
(cyclic? long3) ; true
