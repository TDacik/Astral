(set-info :source Astral)
(set-info :status sat)

(include "predicates/singly_linked_list.smt2")

(assert (ls nil nil))

(check-sat)
