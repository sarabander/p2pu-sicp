
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance)
             ((protected (lambda () balance))))
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

;; I don't see a reason to serialize access to balance. There is only
;; one program step to read the balance variable. It is hardly possible
;; that set! manages to change some bits of balance during the time
;; someone else accesses balance, unless the operations are not atomic.
