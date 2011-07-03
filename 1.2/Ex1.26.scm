;; The multiplication expression evaluates expmod twice, resulting in binary 
;; process tree, which grows exponentially as O(2^n). This cancels the 
;; advantage we had with O(log n) algorithm, because 2^(log n) = n.

