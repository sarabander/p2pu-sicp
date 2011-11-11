
(define (make-account balance password)
  (let ((password-failures 0))
    (define (withdraw amount)
      (set! password-failures 0)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (deposit amount)
      (set! password-failures 0)
      (set! balance (+ balance amount))
      balance)
    (define (block-access amount)
      (set! password-failures (add1 password-failures))
      (if (> password-failures 7) 
	  (call-the-cops)
	  "Incorrect password"))
    (define (call-the-cops)
      "Calling the police... Run!")
    (define (dispatch secret m)
      (cond ((not (eq? secret password)) block-access)
	    ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    (else (error "Unknown request - MAKE-ACCOUNT"
				m))))
    dispatch))

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40) ; 60
((acc '45136 'withdraw) 100) ; "Incorrect password"
((acc '01295 'withdraw) 100) ; (same in between)
((acc 'palmtree 'withdraw) 100)
((acc 'electric 'withdraw) 100)
((acc 'something 'withdraw) 100)
((acc '36012 'withdraw) 100)
((acc '010203 'withdraw) 100) ; "Incorrect password"
;; (7th failure)

((acc 'secret-password 'deposit) 30) ; 90
;; (resets failure counter)

((acc '45136 'withdraw) 100) ; "Incorrect password"
((acc '01295 'withdraw) 100) ; (same in between)
((acc '9754 'withdraw) 100)
((acc 'whatelse 'withdraw) 100)
((acc 'palmtree 'withdraw) 100)
((acc 'electric 'withdraw) 100)
((acc 'something 'withdraw) 100) ; "Incorrect password"
((acc '36012 'withdraw) 100) ; "Calling the police... Run!"
;; (8th failure)
