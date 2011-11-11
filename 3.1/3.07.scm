
;; Unmodified from 3.03
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

;; For creating another access point to existing account
(define (make-joint other-account other-password new-password)
  (define (block-access amount)
    "Incorrect password") 
  (define (dispatch secret m)
    (if (not (eq? secret new-password))
	block-access
	(cond ((eq? m 'withdraw) 
	       (other-account other-password 'withdraw))
	      ((eq? m 'deposit) 
	       (other-account other-password 'deposit))
	      (else (error "Unknown request - MAKE-ACCOUNT"
			   m)))))
  (if (equal? ((other-account other-password 'withdraw) 0)
	      "Incorrect password")
      (begin (printf "Wrong password for existing account\n")
	     "Account doesn't exist")
      dispatch))


;; We make Peter the first user of the new account
(define peter-acc (make-account 100 'open-sesame))

;; We use withdrawal/deposit of zero for showing account balance
((peter-acc 'open-sesame 'withdraw) 0) ; 100
((peter-acc 'secret-password 'withdraw) 40) ; "Incorrect password"
((peter-acc 'open-sesame 'deposit) 20) ; 120


;; Second user of Peter's account
(define paul-acc
  (make-joint peter-acc 'treehouse 'rosebud)) ; [failure]
; => Wrong password for existing account

paul-acc ; "Account doesn't exist"

;; Another attempt
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud)) ; [success]

paul-acc ; #<procedure:dispatch>

((paul-acc 'rosebud 'withdraw) 0) ; 120 
((paul-acc 'sunray 'withdraw) 0) ; "Incorrect password"

((paul-acc 'rosebud 'deposit) 50) ; 170
((paul-acc 'rosebud 'withdraw) 30) ; 140

((peter-acc 'open-sesame 'withdraw) 0) ; 140
((peter-acc 'open-sesame 'withdraw) 50) ; 90

((paul-acc 'rosebud 'deposit) 0) ; 90
((paul-acc 'rosebud 'withdraw) 30) ; 60

((peter-acc 'open-sesame 'withdraw) 30) ; 30

((paul-acc 'rosebud 'withdraw) 0) ; 30


;; Paul shares his account with Mary
(define mary-acc
  (make-joint paul-acc 'hibiscus 'tulip)) ; [failure]
; => Wrong password for existing account

mary-acc ; "Account doesn't exist"

;; Try again
(define mary-acc
  (make-joint paul-acc 'rosebud 'tulip)) ; [success]

mary-acc ; #<procedure:dispatch>

((mary-acc 'winegar 'withdraw) 0) ; "Incorrect password"
((mary-acc 'tulip 'withdraw) 0) ; 30
((mary-acc 'tulip 'deposit) 10) ; 40

((peter-acc 'open-sesame 'withdraw) 0) ; 40
((paul-acc 'rosebud 'deposit) 0) ; 40

((peter-acc 'open-sesame 'withdraw) 5) ; 35
((mary-acc 'tulip 'withdraw) 0) ; 35
