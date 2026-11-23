(set-info :source Astral)
(set-info :status sat)

(include "predicates/singly_linked_list.smt2")

(declare-const x Ref_LS)

(assert
  (sep
    (distinct x nil)
    (ls x nil)
  )
)

(assert (not (pto x nil)))

(check-sat)
