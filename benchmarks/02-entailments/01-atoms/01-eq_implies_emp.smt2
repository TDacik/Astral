(set-info :source Astral)
(set-info :status unsat)

(include "predicates/singly-linked_list.smt2")

(declare-const x Ref_LS)

(assert (= x x))
(assert (not emp))

(check-sat)
