
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
    (let ((protected-withdraw (protected withdraw))
	  (protected-deposit (protected deposit)))
      (define (dispatch m)
	(cond ((eq? m 'withdraw) protected-withdraw)
	      ((eq? m 'deposit) protected-deposit)
	      ((eq? m 'balance) balance)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
			   m))))
      dispatch)))

;; I suspect that this is not entirely safe. Suppose we create a shared
;; account:

(define shared-account (make-account 100))

;; Now, when two people happen to withdraw at the same exact moment:
((shared-account 'withdraw) 50) ; 1st person
((shared-account 'withdraw) 70) ; 2nd person

;; they invoke the same protected-withdraw procedure concurrently. There is
;; no protection against evaluating procedure's subexpressions twice in row.
;; For example, (>= balance amount) could be evaluated by 1st person, then
;; by 2nd person, before 1st person updates balance with set!. Both will
;; proceed under the assumption that there are enough funds.
;; Result is overdraft.

;; One person withdrawing and other depositing concurrently will be safe. 
;; Now they invoke separate procedures that are executed serially.
