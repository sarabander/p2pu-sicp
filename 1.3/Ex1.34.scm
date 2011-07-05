(define (f g)
  (g 2))

;; Before evaluating (f f), I try to predict what happens:

;; The inner scope of f obviously knows about outer environment where f is
;; defined. When supplied with f as an argument, the procedure f recursively 
;; calls itself with argument 2. That in turn causes the 2 to be substituted 
;; in place of g in (g 2), forming (2 2). Scheme tries to evaluate (2 2), but
;; throws an error, because 2 is not an operator. The sequence goes like this:

;; (f f)
;; (f 2)
;; (2 2)
;; => error

;; Let's test:

(f f) ; => procedure application: expected procedure, given: 2; 
;          arguments were: 2

;; Prediction confirmed.
