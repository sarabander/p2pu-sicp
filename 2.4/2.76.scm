
;; I would argue that adding a new operation is easier to the "generic 
;; operations with explicit dispatch" style. There the operation is defined 
;; in one place as a single definition. All the type predicates are listed 
;; there. To make a new operation, one could take an existing operation 
;; definition as a template and implement all the needed selectors and 
;; constructors working on the contents. The amount of work depends on the 
;; number of types in the system. To add a new type to this system requires 
;; finding all the operation definitions, and adding type predicates and 
;; corresponding methods to each of them.

;; Adding a new type is easier to both the data-directed style and message-
;; passing style. In data-directed system we just need to install a new 
;; representation package as a self-contained procedure definition into the
;; table. To add a type to message-passing system, we again need to define
;; just one procedure that encapsulates the dispatch on operation. This 
;; definition could be large if there are many operations, but at least it
;; is all in one place. On the other hand, we need to find all the definitions
;; of message-passing style types and all the type representation packages 
;; from the table to add a new operation to these two systems.
