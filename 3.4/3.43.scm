
;; Sequential processes just swap the account balances, so it is obvious
;; that after any number of exchanges we end up with some permutation of 
;; the original list of balances.

;; Non-sequential exchange still preserves the sum of balances, as long
;; as both withdrawal and deposit succeed or fail together. This way
;; the system is closed -- at any moment the total sum is distributed
;; among accounts and temporary variables (like difference). The amount
;; taken away from one place will be put back in another place. See
;; the first diagram in 3.43.pdf.

;; If withdrawal and deposit are not serialized, we are not guaranteed
;; to possess the latest information about any account's balance. It could
;; change between accessing the balance and setting new balance, as shown
;; on the second diagram in 3.43.pdf. We leak or gain money in the system.
