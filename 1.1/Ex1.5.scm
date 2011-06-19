;; Exercise 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))
(test 0 7)
(test 3 -4)

;; applicative order - arguments will be evaluated first, 
;; then procedure will be applied to them - therefore procedure (p)
;; in (test 0 (p)) will be called immediately. 
;; Result is an infinite loop.
;;
;; normal order - evaluation of the arguments will be postponed
;; until they are needed. The arguments of (test  ) will be passed
;; unevaluated to the procedure body. In the case of (test 0 (p)), 
;; the execution never reaches (p) because of the predicate (= x 0).
