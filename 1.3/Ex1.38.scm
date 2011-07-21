;; Depends on 1.37

(define numer (λ (i) 1.0))

(define denom
  (λ (i) 
     (if (zero? (remainder (+ i 1) 3))
	 (- i (quotient i 3))
	 1)))

;; Just for checking
(map denom (range 1 15)) ; => '(1 2 1 1 4 1 1 6 1 1 8 1 1 10 1)

(define (e depth)
  (+ 2 (cont-frac numer denom depth)))

(e 5)

(map e (range 1 15)) ; =>
;; '(3.0
;;  2.6666666666666665
;;  2.75
;;  2.7142857142857144
;;  2.71875
;;  2.717948717948718
;;  2.7183098591549295
;;  2.718279569892473
;;  2.718283582089552
;;  2.7182817182817183
;;  2.7182818352059925
;;  2.7182818229439496
;;  2.718281828735696
;;  2.7182818284454013
;;  2.718281828470584)

;; Converges pretty quickly to e

;; How much 15-term deep finite cont-frac of e deviates form actual e?
(- (exp 1) (e 15)) ; => -1.154e-11
