(set-info :source Astral)
(set-info :status unsat)

(include "predicates/nested_singly-linked_list.smt2")

(declare-const x1 Ref_NLS)
(declare-const x1_1 Ref_LS)

(assert
  (sep
    (distinct x1_1 nil)
    (pto x1 (c_NLS nil x1_1))
    (ls x1_1 nil)
  )
)


(assert (not (nls x1 nil nil)))

(check-sat)
