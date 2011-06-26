;; Prints the Pascal's triangle. 16 rows fit well.
;; For more compact triangles, see Ex1.12a.scm
;;
;;               j
;;      | 0  1  2  3  4  5
;;    __|_________________
;;    0 | 1  
;;    1 | 1  1
;;  i 2 | 1  2  1
;;    3 | 1  3  3  1
;;    4 | 1  4  6  4  1
;;    5 | 1  5 10 10  5  1

;; Gives an element of Pascal's triangle
(define (pascal i j) ; i: row index, j: column index
  (cond	((or (< i 0) (< j 0) (< i j)) 0)
	((or (= j 0) (= i j)) 1)
	(else (+ (pascal (- i 1) (- j 1))
		 (pascal (- i 1) j)))))

(pascal 0 2)
(pascal 3 0)
(pascal 3 1)
(pascal 4 2)
(pascal 5 4)

;; Prints elements of a single row starting with element having
;; column index j = <first-elem> until j = <last-elem>.
;; <last-elem> determines, which row we deal with.
(define (printrow first-elem last-elem) 
  (cond ((> first-elem last-elem) (display "\n"))
	(else (let ((elem (pascal last-elem first-elem)))
		(and (display elem)
		     (cond ((> elem 999) (repeat " " 2))
			   ((> elem  99) (repeat " " 3))
			   ((> elem   9) (repeat " " 4))
			   (else (repeat " " 5)))
		     (printrow (+ first-elem 1) last-elem))))))

(printrow 0 5) ; elements of 5th row from 0 to 5

(pascal 5 0)   ; 1st element of 5th row
(printrow 1 5) ; rest of the elements of 5th row

(printrow 0 2) ; elements of 2nd row from 0 to 2

;; Prints triangle rows from <first> to <last>,
;; counting starts from zero
(define (trianglerows first last)
  (cond ((or (< first 0) (< last 0) (> first last)) 
	 (repeat "-" (+ (* 6 last) 1)))
	(else (and (repeat "   " (- last first))
		   (printrow 0 first) 
		   (trianglerows (+ first 1) last)))))

(define (repeat string n-times) 
  (if (< n-times 1)
      (void)
      (and (display string)
	   (repeat string (- n-times 1)))))

(repeat "\n" 3)

(and (display "\n")
     (trianglerows 0 15)
     (display "\n"))
