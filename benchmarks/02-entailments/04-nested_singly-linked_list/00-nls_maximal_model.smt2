(set-info :source Astral)
(set-info :status sat)

(include "predicates/nested_singly-linked_list.smt2")

(declare-const x Ref_NLS)

(assert
  (sep
    (distinct x nil)
    (nls x nil nil)
  )
)

(assert (not (pto x (c_NLS nil nil))))

(check-sat)
