(set-info :source Astral)
(set-info :status unsat)

(include "predicates/singly-linked_list.smt2")

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert (pto x (c_LS y)))
(assert (not (pto x (c_LS z))))

(check-sat)
