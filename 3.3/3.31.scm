
;; Modified 
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      ;; missing (proc) call
      )
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

;; Without running the action procedure right away, the agenda is not 
;; updated. For example, the invert-input procedure in inverter below 
;; needs to be called explicitly. Otherwise after-delay is not run, 
;; and the lambda-expression with set-signal! is not added to the
;; agenda. Therefore propagate doesn't update the output signal:

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;; An example of resulting inverter behaviour:

(define input (make-wire))
(define output (make-wire))

(inverter input output)

(get-signal input)  ; 0
(get-signal output) ; 0

(propagate)

(get-signal input)  ; 0
(get-signal output) ; 0

;; Correct value didn't propagate to the output.

(set-signal! input 0)
(propagate)

(get-signal output) ; 0

;; Still incorrect

(set-signal! input 1)
(propagate)
(set-signal! input 0)
(propagate)

(get-signal output) ; 1

;; Finally correct.

;; Proper make-wire:
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))  ; important call!
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

;; Example of correct inverter behaviour:

(define input (make-wire))
(define output (make-wire))

(inverter input output)

(get-signal input)  ; 0
(get-signal output) ; 0

(propagate)

(get-signal input)  ; 0
(get-signal output) ; 1

(set-signal! input 1)
(propagate)

(get-signal input)  ; 1
(get-signal output) ; 0
