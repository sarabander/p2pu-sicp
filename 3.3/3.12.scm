
;; Works in MIT-Scheme

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z ; '(a b c d)

(cdr x) ; => '(b)

(define w (append! x y))

w ; '(a b c d)
x ; '(a b c d)

(cdr x) ; => '(b c d)

;; First we have two lists:
;; 
;;  x    ╭───┬───╮   ╭───┬───╮      y    ╭───┬───╮   ╭───┬───╮ 
;;  ────>│ ∘ │ ∘─┼──>│ ∘ │ ╱ │      ────>│ ∘ │ ∘─┼──>│ ∘ │ ╱ │ 
;;       ╰─┼─┴───╯   ╰─┼─┴───╯           ╰─┼─┴───╯   ╰─┼─┴───╯ 
;;         │           │                   │           │ 
;;       ╭─V─╮       ╭─V─╮               ╭─V─╮       ╭─V─╮
;;       │ a │       │ b │               │ c │       │ d │
;;       ╰───╯       ╰───╯               ╰───╯       ╰───╯
;;
;; Normal append creates a new list, leaving x and y unmodified:
;; 
;;  z    ╭───┬───╮   ╭───┬───╮   ╭───┬───╮   ╭───┬───╮ 
;;  ────>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ ╱ │ 
;;       ╰─┼─┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯ 
;;         │           │           │           │ 
;;       ╭─V─╮       ╭─V─╮       ╭─V─╮       ╭─V─╮
;;       │ a │       │ b │       │ c │       │ d │
;;       ╰───╯       ╰───╯       ╰───╯       ╰───╯
;;
;; Mutating append! points the last pair of x to y, but leaves y intact:

;;        w │                     y │
;;          │                       │
;;  x    ╭──V┬───╮   ╭───┬───╮   ╭──V┬───╮   ╭───┬───╮ 
;;  ────>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ ∘─┼──>│ ∘ │ ╱ │ 
;;       ╰─┼─┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯   ╰─┼─┴───╯ 
;;         │           │           │           │ 
;;       ╭─V─╮       ╭─V─╮       ╭─V─╮       ╭─V─╮
;;       │ a │       │ b │       │ c │       │ d │
;;       ╰───╯       ╰───╯       ╰───╯       ╰───╯
;;
;; The same structure is also accessible through w.
