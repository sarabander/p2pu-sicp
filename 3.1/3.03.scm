
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (block-access amount)
    "Incorrect password")
  (define (dispatch secret m)
    (if (not (eq? secret password))
	block-access
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request - MAKE-ACCOUNT"
			   m)))))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40) ; 60
((acc 'secret-password 'withdraw) 40) ; 20
((acc 'some-other-password 'deposit) 50) ; "Incorrect password"
((acc 'secret-password 'withdraw) 25) ; "Insufficient funds"
((acc 'secret-password 'withdraw) 10) ; 10
((acc '45136 'deposit) 100) ; "Incorrect password"
((acc 'secret-password 'deposit) 100) ; 110
((acc 'secret-password 'deposit) 50) ; 160
((acc 'secret-password 'withdraw) 250) ; "Insufficient funds"
