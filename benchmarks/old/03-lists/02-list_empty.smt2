(set-info :source Astral)
(set-info :status sat)

(include "predicates/singly_linked_list.smt2")

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert
  (sep
    (= x y)
    (ls x y)
  )
)

(check-sat)
