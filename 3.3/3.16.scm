
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

(define p (list 'a 'b 'c))
(count-pairs p) ; 3

;; 
;;  p    ╭───┬───╮   ╭───┬───╮   ╭───┬───╮ 
;;  ────>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ ╱ │ 
;;       ╰─┼─┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯ 
;;         │           │           │ 
;;       ╭─V─╮       ╭─V─╮       ╭─V─╮
;;       │ a │       │ b │       │ c │
;;       ╰───╯       ╰───╯       ╰───╯
;;

(define q '(a b c))
(set-car! q (cddr q))
(count-pairs q) ; 4

q ;Value 28: ((c) b c)
;;
;;         ╭────────────────────────╮
;;         │                        │
;;  q    ╭─┼─┬───╮   ╭───┬───╮   ╭──V┬───╮
;;  ────>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ / │
;;       ╰───┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯ 
;;                     │           │ 
;;                   ╭─V─╮       ╭─V─╮ 
;;                   │ b │       │ c │ 
;;                   ╰───╯       ╰───╯ 
;;

(define r '(a b c))
(set-car! r (cdr r))
(set-car! (cdr r) (cddr r))
(count-pairs r) ; 7

r ;Value 27: (((c) c) (c) c)
;; 
;;  r    ╭───┬───╮
;;  ────>│ ∘ │ ∘ │
;;       ╰─┼─┴─┼─╯
;;         │   │
;;       ╭─V─┬─V─╮
;;       │ ∘ │ ∘ │
;;       ╰─┼─┴─┼─╯
;;         │   │
;;       ╭─V─┬─V─╮
;;       │ ∘ │ / │
;;       ╰─┼─┴───╯
;;         │
;;       ╭─V─╮ 
;;       │ c │ 
;;       ╰───╯ 
;;

;; Any list structure containing a loop would not return. For example:
(define s '(a b c))
(set-car! (cdr s) s)
(count-pairs s) ;Aborting!: maximum recursion depth exceeded

s ;Value 24: (a (a (a (a ...
(cdr s) ;Value 29: ((a (a (a (a ...
;;
;;          ╭──────────╮
;;          │          │
;;  s    ╭──V┬───╮   ╭─┼─┬───╮   ╭───┬───╮
;;  ────>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ / │
;;       ╰─┼─┴───╯   ╰───┴───╯   ╰─┼─┴───╯ 
;;         │                       │ 
;;       ╭─V─╮                   ╭─V─╮ 
;;       │ a │                   │ c │ 
;;       ╰───╯                   ╰───╯ 
;;

