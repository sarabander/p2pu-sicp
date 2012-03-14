
;; Depends on make-wire, call-each and others defined in ch3.scm:
;; http://mitpress.mit.edu/sicp/code/ch3.scm

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; For testing full-adder
(define input1 (make-wire))
(define input2 (make-wire))
(define carry-in (make-wire))
(define sum-out (make-wire))
(define carry-out (make-wire))

(input1 'get-signal)    ; 0
(input2 'get-signal)    ; 0
(carry-in 'get-signal)  ; 0
(sum-out 'get-signal)   ; 0
(carry-out 'get-signal) ; 0

(full-adder input1 input2 carry-in sum-out carry-out)

((input1 'set-signal!) 1)
(propagate)
(sum-out 'get-signal)   ; 1
(carry-out 'get-signal) ; 0

((input2 'set-signal!) 1)
(propagate)
(sum-out 'get-signal)   ; 0
(carry-out 'get-signal) ; 1
;; Correct.

;; The RCA
;; --------------------------------------------------
(define (ripple-carry-adder a-list b-list sum-list c)
  (let ((carry (make-wire)))
    (cond ((null? (cdr a-list))
	   (set-signal! carry 0))
	  (else	(ripple-carry-adder (cdr a-list)
				    (cdr b-list)
				    (cdr sum-list)
				    carry)))
    (full-adder (car a-list)
		(car b-list)
		carry
		(car sum-list)
		c)
    'ok))
;; --------------------------------------------------

;; Test with 1 bit
(ripple-carry-adder (list input1) (list input2) (list sum-out) carry-out)

((input1 'set-signal!) 1)
(sum-out 'get-signal)   ; 0
(carry-out 'get-signal) ; 0

(propagate)

(sum-out 'get-signal)   ; 1
(carry-out 'get-signal) ; 0

((input2 'set-signal!) 1)

(propagate)
(sum-out 'get-signal)   ; 0
(carry-out 'get-signal) ; 1
;; Correct.

;; For wires bundled into a bus
(define (make-bus width)
  (if (zero? width)
      '()
      (cons (make-wire) 
            (make-bus (- width 1)))))

(define (get-bus-signals bus)
  (if (null? bus)
      '()
      (cons ((car bus) 'get-signal)
            (get-bus-signals (cdr bus)))))

(define (set-bus-signals! bus signal-list)
  (define (update-bus bs lst)
    (if (null? bs)
        'done
        (begin
          (((car bs) 'set-signal!) (car lst))
          (update-bus (cdr bs) (cdr lst)))))
  (cond ((not (= (length bus) (length signal-list)))
         (error "Bus width and signal list width must be equal."))
        (else
         (update-bus bus signal-list))))
        
;; Test with 4 bits
(define a-vector (make-bus 4))
(define b-vector (make-bus 4))
(define sum-vector (make-bus 4))
(define carry-signal (make-wire))
(get-bus-signals a-vector) ; (0 0 0 0)
(set-bus-signals! a-vector  '(1 0 1 0))
(get-bus-signals a-vector) ; (1 0 1 0)

(set-bus-signals! a-vector  '(1 1 1 1))
(get-bus-signals a-vector) ; (1 1 1 1)

(ripple-carry-adder a-vector b-vector sum-vector carry-signal)

(set-bus-signals! a-vector  '(0 0 0 1))
(set-bus-signals! b-vector  '(0 0 0 1))
(propagate)
(get-bus-signals sum-vector) ; (0 0 1 0)
(carry-signal 'get-signal) ; 0

(set-bus-signals! a-vector  '(0 1 1 1))
(set-bus-signals! b-vector  '(0 0 0 1))
(propagate)
(get-bus-signals sum-vector) ; (1 0 0 0)

(set-bus-signals! a-vector  '(0 1 1 1))
(set-bus-signals! b-vector  '(0 1 1 1))
(propagate)
(get-bus-signals sum-vector) ; (1 1 1 0)

(set-bus-signals! a-vector  '(0 1 1 1))
(set-bus-signals! b-vector  '(1 0 0 1))
(propagate)
(get-bus-signals sum-vector) ; (0 0 0 0)
(carry-signal 'get-signal) ; 1
;; All correct.

;; Delay calculations
;; ------------------------------------------------
;; Half-adder delay: ha = max(or, and + inv) + and.
;; Full-adder delay: fa = 2*ha + or.
;;
;; n-bit RCA delay: n*fa = n*(2*ha + or)
;; = n*(2*(max(or, and + inv) + and) + or)
;; ------------------------------------------------

;; Now we will test with arbitrary positive integers

;; Base converters
(define (dec->bin decimal)
  (define (dec->bin-1 decimal)
    (if (zero? decimal)
	'()
	(cons (remainder decimal 2)
	      (dec->bin-1 (quotient decimal 2)))))
  (if (zero? decimal)
      (list 0)
      (reverse (dec->bin-1 decimal))))

(define (bin->dec binary)
  (let ((position (- (length binary) 1)))
    (if (zero? position)
        (car binary)
        (+ (* (expt 2 position) (car binary))
           (bin->dec (cdr binary))))))

;; Adds two positive decimal numbers using RCA
(define (add a b)
  ;; Pads list with zeros from left until width is reached:
  (define (widen lst width)
    (if (= (length lst) width)
        lst
        (widen (cons 0 lst) width)))
  (let* ((a-bin (dec->bin a))
         (b-bin (dec->bin b))
         (wide-enough (+ 1 (max (length a-bin) (length b-bin))))
         (a-bin-wider (widen a-bin wide-enough))
         (b-bin-wider (widen b-bin wide-enough))
         (a-vector (make-bus wide-enough))
         (b-vector (make-bus wide-enough))
         (sum-vector (make-bus wide-enough))
         (carry-pin (make-wire)))
    (set-bus-signals! a-vector a-bin-wider)
    (set-bus-signals! b-vector b-bin-wider)
    (ripple-carry-adder a-vector b-vector sum-vector carry-pin)
    (propagate)
    (bin->dec (get-bus-signals sum-vector))))

(add 0 0) ; 0
(add 1 0) ; 1
(add 10 1) ; 11
(add 7 9) ; 16
(add 42 12) ; 54
(add 64 720) ; 784
(add 1024 4096) ; 5120

(- (add 45633041 79604103001405)
   (+   45633041 79604103001405)) ; 0

(- (add (expt 2 1024) (expt 2 1008))
   (+   (expt 2 1024) (expt 2 1008))) ; 0
