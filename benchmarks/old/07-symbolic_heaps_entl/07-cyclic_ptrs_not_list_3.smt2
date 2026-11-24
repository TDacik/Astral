(set-info :source Astral)
(set-info :status sat)

(include "predicates/singly_linked_list.smt2")

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert
  (sep
    (pto x y)
    (pto y z)
    (distinct x z)
  )
)

(assert (not (ls x z)))

(check-sat)
