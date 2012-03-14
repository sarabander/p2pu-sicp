
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

;; Or-gate can be built by inverting both inputs 
;; of the and-gate, and inverting its output:
(define (or-gate in1 in2 out)
  (let ((not-in1 (make-wire)) 
	(not-in2 (make-wire))
	(and-out (make-wire)))
    (inverter in1 not-in1)
    (inverter in2 not-in2)
    (and-gate not-in1 not-in1 and-out)
    (inverter and-out out)
    'ok))

;; Delay time = 2 * inverter-delay + and-gate-delay.
