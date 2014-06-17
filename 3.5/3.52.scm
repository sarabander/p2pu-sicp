
(define sum 0)

sum ;=> 0

(define (accum x)
  (set! sum (+ x sum))
  sum)

sum ;=> 0

(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))

; The definition of seq executes procedure accum once to produce the first
; element of the mapped stream and delays the rest of the stream. So sum
; is set to the first element of (stream-enumerate-interval 1 20), that is 1.
sum ;=> 1

(define y (stream-filter even? seq))

; Stream-filter probes the elements of seq one-by-one. The first element is
; 1 (not even), so it forces the next element. This in turn forces the
; next element of (stream-enumerate-interval 1 20), which is 2. Accum
; executes second time, setting sum to 1 + 2 = 3. Now seq has two forced
; elements: (1 3 ...). Stream-filter probes 3, which is also not even, so
; it requests the next element of seq. This trickles down to the enumerator,
; producing 3. Accum adds this to sum: 3 + 3 = 6. Seq has three forced
; elements now: (1 3 6 ...). Stream-filter tests 6 (even) and is satisfied.
; No more forcings of further elements of seq happens, sum contains the
; last forced element of seq, and y is a stream starting with 6.
sum ;=> 6

(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0))
   seq))

; Z is a stream consisting of those elements of seq that are divisible by 5.
; Stream-filter drops the first three elements of seq (1, 3, and 6) and
; forces the calculation of fourth element. This forces the enumerator to
; deliver 4 that accum adds to sum: 6 + 4 = 10. Seq is now (1 3 6 10 ...).
; 10 passes the filter and z is now (10 ...).
sum ;=> 10

(stream-ref y 7) ;=> 136

; In order to produce 8 elements of y, seq has to force itself this far:
; (1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 ...). Y retains only
; even elements from seq and is now (6 10 28 36 66 78 120 136 ...).
sum ;=> 136

(display-stream z)
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210'done

; We want to print all of z -- that is every element of seq divisible by 5.
; Seq consists of 20 elements, because our enumerator generates 1..20.
; seq = (1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210).

sum ;=> 210

;-----------------------------------------------------------------------------

; The results using non-memoized delay. Responses will differ starting
; with the definition of stream z.

(define sum 0)

sum ;=> 0

(define (accum x)
  (set! sum (+ x sum))
  sum)

sum ;=> 0

(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))

sum ;=> 1

(define y (stream-filter even? seq))

sum ;=> 6

(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0))
   seq))

sum ;=> 15

; The following commented-out expressions are here to illustrate the state
; of seq and y at this point. The evaluations are destructive, so we must
; re-initialize sum and re-evaluate all the uncommented expressions so far.

; By defining z, we traverse seq again, but it is not the same any more!
; It keeps changing every time we walk through it. And the amount of change
; depends on how far we go into the stream.

;; The original seq was (1 3 6 10 ...), but now if we read it:
;(print-n seq 4)
;; 1, 8, 11, 15, ... 

; The fourth element (15) passes the filter, z is now (15 ...) and sum is 15.

;; If we were to print seq second time, it would be mutated again:
;(print-n seq 4)
;; 1, 22, 25, 29, ... 

;; We could peek the first eight elements of y, but doing so would alter seq.
;(print-n y 8)
;; 6, 24, 30, 54, 64, 100, 114, 162, ...  

(stream-ref y 7) ;=> 162

sum ;=> 162

(display-stream z)
; 15
; 180
; 230
; 305'done

sum ;=> 362

; All our streams in this exercise are intrinsically mutating under our feet
; every time we touch them in some way, even by just looking at them.

; If we display z again, we get different result, as would be expected:
(display-stream z)
;15
;380
;430
;505'done

; Content of our streams here depend on the evaluation order.
; All our trouble stems from mixing mutation with lazy sequences.

