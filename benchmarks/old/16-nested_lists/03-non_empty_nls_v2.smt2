(set-info :source Astral)
(set-info :status sat)

(include "predicates/nested_singly_linked_list.smt2")

(declare-const x Ref_NLS)
(declare-const y Ref_NLS)

(assert
  (sep
    (distinct x y nil)
    (nls x y nil)
  )
)

(check-sat)
