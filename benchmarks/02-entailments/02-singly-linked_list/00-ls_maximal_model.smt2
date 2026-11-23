(set-info :source Astral)
(set-info :status sat)

(include "predicates/singly-linked_list.smt2")

(declare-const x Ref_LS)

(assert
  (sep
    (distinct x nil)
    (ls x nil)
  )
)

(assert (not (pto x (c_LS nil))))

(check-sat)
