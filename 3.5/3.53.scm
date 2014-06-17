
(define s (cons-stream 1 (add-streams s s)))

;   1 2 4  8 16 ... = s
; + 1 2 4  8 16 ... = s
; -----------------
; 1 2 4 8 16 32 ... = s

; Next element is twice the previous, except first.
; This gives us powers of 2.

; Check:
(print-n s 14)
;=> 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, ... 

