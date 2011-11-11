;; a.

;; Calculates recursively by descending deeper
(define (cont-frac n d k)
  (define (recur i)
    (if (= i k)
	(/ (n i) (d i))
	(/ (n i) (+ (d i) (recur (add1 i))))))
  (recur 1))

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   5) ; => 0.625

(define (range first last)
  (cond ((> first last) '())
	(else (cons first (range (+ first 1) last)))))

(map (λ (x) (cont-frac (lambda (i) 1.0)
		       (lambda (i) 1.0)
		       x))
     (range 1 12))

; '(1.0
;   0.5
;   0.6666666666666666
;   0.6000000000000001
;   0.625
;   0.6153846153846154
;   0.6190476190476191
;   0.6176470588235294
;   0.6181818181818182
;   0.6179775280898876
;   0.6180555555555556
;   0.6180257510729613)

;; To get 4 correct decimal places of 1/𝛗, which is 0.6180,
;; we should use 11-term finite continued fraction.

;; b.

;; Iterates by climbing up
(define (cont-frac n d k)
  (define (iter numer denom i)
    (if (= i 1)
	(/ numer denom)
	(iter (n (sub1 i)) 
	      (+ (d (sub1 i)) (/ numer denom)) 
	      (sub1 i))))
  (iter (n k) (d k) k))
