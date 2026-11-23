(set-info :source Astral)
(set-info :status sat)

(include "predicates/doubly_linked_list.smt2")

(declare-const x Ref_DLS)
(declare-const y Ref_DLS)

(assert (pto x (c_DLS nil nil)))
(assert (not (dls x y nil nil)))

(check-sat)
