(set-info :source Astral)
(set-info :status unsat)

(include "predicates/singly-linked_list.smt2")

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert (pto x (c_LS nil)))

(assert (not (ls x nil)))

(check-sat)
