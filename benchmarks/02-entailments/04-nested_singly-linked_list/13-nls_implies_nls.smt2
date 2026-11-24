(set-info :source Astral)
(set-info :status unsat)

(include "predicates/nested_singly-linked_list.smt2")

(declare-const x Ref_NLS)
(declare-const y Ref_NLS)

(assert (nls x y nil))
(assert (not (nls x y nil)))

(check-sat)
