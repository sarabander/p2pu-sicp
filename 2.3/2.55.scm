
(car ''abracadabra)

;; What this really means, is:
(car (quote (quote abracadabra)))

;; or alternatively:
(car '(quote abracadabra)) ; which gives back 'quote
