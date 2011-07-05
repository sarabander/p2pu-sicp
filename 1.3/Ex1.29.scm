;; Simpson’s Rule

(define (square x) (* x x))

(define (cube x) (* x x x))

;; recursive sum
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (define (add-h x) (+ x h))
  (define h (/ (- b a) n))
  (define (summand x) 
    (define multiplier ; to make term multipliers like 1, 4, 2, 4, ...
      (cond ((or (= x a) (= x b)) 1)
	    ((odd? (round (/ (- x a) h))) 4) ; if index k is odd
	    (else 2)))
    (* multiplier (f x)))
  (* (/ h 3) (sum summand a add-h b)))

;; gives exact answers with integer arguments and even n:
(simpson-integral cube 0 1 100)  ; 1/4
(simpson-integral cube 0 1 134)  ; 1/4
(simpson-integral cube 0 1 1000) ; 1/4

;; odd n gives rational numbers, loses accuracy:
(simpson-integral cube 0 1 47) ; 1186417/4879681 is approx 0.24313413110406193

;; not very accurate with floating-point numbers and small n:
(simpson-integral cube 0 1 100.0)    ; 0.24666666666666687
(simpson-integral cube 0 1 1000.0)   ; 0.24966666666666754
(simpson-integral cube 0 1 10000.0)  ; 0.2500333333332677
(simpson-integral cube 0 1 100000.0) ; 0.2500033333324631

(simpson-integral cube 0 4 100) ; 64

(simpson-integral square 0 1 100) ; 1/3
(simpson-integral square 0 3 100) ; 9
(simpson-integral square 0 6 100) ; 72
(simpson-integral square 0 9 100) ; 243

(simpson-integral square 0 1.1 100) ; 0.448
