(set-info :source Astral)
(set-info :status unsat)

(include "predicates/doubly_linked_list.smt2")

(declare-const x Ref_DLS)

(assert (pto x (c_DLS nil nil)))
(assert (not (dls x x nil nil)))

(check-sat)
