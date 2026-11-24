(set-info :source Astral)
(set-info :status sat)

(include "predicates/singly_linked_list.smt2")

(declare-const x Ref_LS)
(declare-const y1 Ref_LS)
(declare-const y2 Ref_LS)

(assert
  (sep
    (distinct y1 y2)
    (ls x y1)
    (ls x y2)
  )
)

(check-sat)
