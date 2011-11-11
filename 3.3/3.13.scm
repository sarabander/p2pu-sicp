
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(last-pair z)
;; This goes to infinite loop: there is no last pair, because z is a 
;; cyclical linked list:
;;
;;          ╭────────────────────────────────╮
;;          │                                │
;;  z    ╭──V┬───╮   ╭───┬───╮   ╭───┬───╮   │
;;  ────>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼───╯
;;       ╰─┼─┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯ 
;;         │           │           │ 
;;       ╭─V─╮       ╭─V─╮       ╭─V─╮ 
;;       │ a │       │ b │       │ c │ 
;;       ╰───╯       ╰───╯       ╰───╯ 
;;

;; Evaluation of z produces never-ending sequence: (a b c a b c a b ...
