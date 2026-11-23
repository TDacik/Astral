(set-info :source Astral)
(set-info :status sat)

(include "predicates/singly-linked_list.smt2")

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert emp)

(assert (not (ls x y)))

(check-sat)
