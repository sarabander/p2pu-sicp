
;; Suppose we have two divisions with different personnel files:

(define division1
  '(div1 
    ("Mark Morning" (address "23 Sunny St.") (salary 1240)) 
    ("Edna Evening" (address "41 Moon St.")  (salary 1710))))

(define division2
  '(div2 
    ((name "Kelly Gardner") (wage 1305) (addr "12 Main Rd.")) 
    ((name "Suzie Bee")     (wage 2100) (addr "37 Main Rd."))
    ((name "Joseph McCoy")  (wage 1530) (addr "3 Lakeside Blvd."))))

;; A hash table to hold file reading operations:
(define file-ops (make-hash))

;; Getter and setter to access the hash table:
(define (retrieve-op op type)
  (hash-ref file-ops (list op type) false))
(define (set-op op type proc)
  (hash-set! file-ops (list op type) proc))

;; a. 

;; Generic procedure to retrieve employee's record from any file:
(define (get-record name file)
  ((retrieve-op 'employee (type-tag file)) name file))

;; Populating the table with procedures:
(set-op 'employee 'div1
	(λ (name file)
	   (let ((result (filter (λ (emprecord) 
				    (equal? (car emprecord) 
					    name)) 
				 (cdr file))))
	     (if (empty? result)
		 false
		 (car result)))))

(set-op 'employee 'div2
	(λ (name file)
	   (let ((result (filter (λ (emprecord) 
				    (equal? (cadar emprecord) 
					    name)) 
				 (cdr file))))
	     (if (empty? result)
		 false
		 (car result)))))

;; Tests
(get-record "Mark Morning" division1) ; 8-D
(get-record "Mark Night"   division1) ; :-(
(get-record "Edna Evening" division1) ; 8-D
(get-record "Mark Morning" division2) ; :-(

(get-record "Suzie Quattro" division2) ; :-(
(get-record "Suzie Bee"     division2) ; 8-D
(get-record "Kelly Gardner" division2) ; 8-D
(get-record "Joseph McCoy"  division2) ; 8-D

;; b. 

;; Generic procedure to retrieve employee's salary from any file:
(define (get-salary name file)
  ((retrieve-op 'salary (type-tag file)) name file))

;; Populating the table with procedures:
(set-op 'salary 'div1
	(λ (name file)
	   (let ((result (filter (λ (emprecord) 
				    (equal? (car emprecord) 
					    name)) 
				 (cdr file))))
	     (if (empty? result)
		 false
		 (cadar (filter (λ (field) (eq? (car field)
					       'salary))
			       (cdar result)))))))

(set-op 'salary 'div2
	(λ (name file)
	   (let ((result (filter (λ (emprecord) 
				    (equal? (cadar emprecord) 
					    name)) 
				 (cdr file))))
	     (if (empty? result)
		 false
		 (cadar (filter (λ (field) (eq? (car field)
					       'wage))
			       (car result)))))))

;; Tests
(get-salary "Mark Morning" division1) ; 1240
(get-salary "Mark Night"   division1) ; employee not found
(get-salary "Edna Evening" division1) ; 1710
(get-salary "Mark Morning" division2) ; employee not found

(get-salary "Frodo Baggins" division2) ; employee not found
(get-salary "Suzie Bee"     division2) ; 2100
(get-salary "Kelly Gardner" division2) ; 1305
(get-salary "Joseph McCoy"  division2) ; 1530

;; c.

(define (true? x) (not (false? x)))

(define (find-employee-record name files)
  (let ((result (filter true? 
			(map (λ (file) (get-record name file)) 
			     files))))
    (if (empty? result)
	"Employee not found"
	(car result))))


(find-employee-record "Bilbo" (list division1 division2))
; "Employee not found"
(find-employee-record "Kelly Gardner" (list division1 division2)) ; found
(find-employee-record "Mark Morning" (list division1 division2)) ; found

;; d. 

;; We only must add one procedure per accessor operation (like get-record
;; or get-salary) to the hashtable.
