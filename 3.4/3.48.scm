
;; Works in Racket
;; (http://stackoverflow.com/questions/13467753/implement-parallel-execute-in-scheme)
(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))

;; For MIT Scheme, use parallel-execute from parallel.scm.

;; Other dependencies

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;; Exercise starts here

(define (make-serial number)
  (lambda ()
    (set! number (+ number 1))
    number))

;; Initialize new counter to generate unique account numbers each time
;; (serial-no) is called.
(define serial-no (make-serial 0))

;; Added account number as a parameter.
(define (make-account-and-serializer number balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'number) number)  ; new message 
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

;; Helper functions to determine lower- and higher-numbered account.
(define (compare pred acc1 acc2)  ; pred would be < or >
    (if (pred (acc1 'number) (acc2 'number))
        acc1
        acc2))

(define (lower acc1 acc2)
  (compare < acc1 acc2))

(define (higher acc1 acc2)
  (compare > acc1 acc2))

;; Acquires mutexes in the order of increasing account numbers.
(define (serialized-exchange account1 account2)
  (let ((serializer1 ((lower account1 account2) 'serializer))
        (serializer2 ((higher account1 account2) 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

;; Initialize two accounts to be exchanged.
(define acc1 (make-account-and-serializer (serial-no) 25))
(define acc2 (make-account-and-serializer (serial-no) 100))

(acc1 'balance)
(acc1 'number)

(acc2 'balance)
(acc2 'number)

;; Attempt to exchange the account balances from both ends at the same time.
(parallel-execute
 (lambda () (serialized-exchange acc1 acc2))
 (lambda () (serialized-exchange acc2 acc1)))
;; Result is indistinguishable from the initial situation.

;; This method of acquiring the accounts in the order of account numbers
;; forces the mutexes to start the acquisition from the same account.
;; Who gets there first secures both first and second account, and the
;; other one must wait until the first account is unlocked again.
;; Neither serialized-exchange process attempts to lock the higher-numbered
;; account if it was unsuccessful in acquiring the lower-numbered account.
;; Deadlock doesn't occur because both acquisitions move in the same
;; direction, never in opposite directions.
