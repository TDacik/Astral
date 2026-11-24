(set-info :source Astral)
(set-info :status unsat)

(include "predicates/doubly-linked_list.smt2")

(declare-const x Ref_DLS)
(declare-const y Ref_DLS)

(assert (dls x y nil nil))
(assert (not (dls x y nil nil)))

(check-sat)
