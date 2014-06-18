
(define (sqrt-stream x)
  (cons-stream 
   1.0
   (stream-map (lambda (guess)
                 (sqrt-improve guess x))
               (sqrt-stream x))))

; This procedure creates a brand new stream each time it is recursively called.
; All the 'sqrt-improve' computations will be run redundantly over and over.
; There is no remembering of previous results as with the other version with
; local variable 'guesses'. The latter creates only one stream that remembers
; the results of already run slices of itself thanks to memoization.

; If we don't use memoization, then both versions will recompute all the
; stream elements. In addition to that the new 'sqrt-stream' will be less
; space-efficient due to creating new streams each time, while the procedure
; with 'guesses' refers to a single stream.

