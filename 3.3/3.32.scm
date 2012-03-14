
;; The procedures to be studied, for reference:
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

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

;; Added message to show action-procedures list
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            ((eq? m 'get-actions) action-procedures)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

;; Tracing action lists of the wires and the agenda:
(define the-agenda (make-agenda))
the-agenda ; (0)

(define and-gate-delay 3)

(define in1 (make-wire))
(in1 'get-actions) ; ()

(define in2 (make-wire))
(in2 'get-actions) ; ()

(define and-out (make-wire))
(and-out 'get-signal) ; 0

(and-gate in1 in2 and-out)
(in1 'get-actions) ; (#[compound-procedure 92 and-action-procedure])
(in2 'get-actions) ; (#[compound-procedure 92 and-action-procedure])

the-agenda ; (0 (3 (#[compound-procedure 101] #[compound-procedure 102]) #[compound-procedure 102]))

(propagate)
the-agenda ; (3)
(and-out 'get-signal) ; 0

(set-signal! in1 1)
the-agenda ; (3 (6 (#[compound-procedure 103]) #[compound-procedure 103]))

(set-signal! in2 1)
the-agenda ; (3 (6 (#[compound-procedure 103] #[compound-procedure 104]) #[compound-procedure 104]))

(propagate)
the-agenda ; (6)
(and-out 'get-signal) ; 1

;; Transition 0, 1 --> 1, 0
(set-signal! in1 0)
(set-signal! in2 1)
the-agenda ; (6 (9 (#[compound-procedure 105]) #[compound-procedure 105]))

(set-signal! in1 1)
(set-signal! in2 0)
the-agenda ; (6 (9 (#[compound-procedure 105] #[compound-procedure 106] #[compound-procedure 107]) #[compound-procedure 107]))

(propagate)
the-agenda ; (9)

(and-out 'get-signal) ; 0

;; If we were to store the actions in a list, then the final state of
;; the inputs would be the configuration we added to the list first. 
;; So the chronology of the events would be reversed. 

;; Suppose we first set inputs to (0, 1), later to (1, 0), then after 
;; executing the agenda, we end up with (0, 1), which is clearly wrong. 
;; So we better use queue, not stack (implemented as list).
