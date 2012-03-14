
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

;; There is no problem with transfer as long as the transaction is 
;; guaranteed to succeed or fail atomically. Transfer differs from 
;; exchange such that we don't need to swap the account balances,
;; and we don't expect the sum of the balances to remain the same
;; after the transaction.
