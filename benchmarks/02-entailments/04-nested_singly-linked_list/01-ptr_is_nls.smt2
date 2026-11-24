(set-info :source Astral)
(set-info :status unsat)

(include "predicates/nested_singly-linked_list.smt2")

(declare-const x1 Ref_NLS)
(declare-const x2 Ref_NLS)

(assert
  (sep
    (distinct x1 x2)
    (pto x1 (c_NLS x2 nil))
  )
)

(assert (not (nls x1 x2 nil)))

(check-sat)
