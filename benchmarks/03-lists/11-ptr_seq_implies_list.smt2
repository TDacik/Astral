(set-info :source Astral)
(set-info :status sat)

(include "predicates/singly_linked_list.smt2")

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const a Ref_LS)
(declare-const b Ref_LS)

(assert
  (sep
    (distinct x y)
    (pto x a)
    (pto a b)
    (pto b y)
  )
)

(assert (not (ls x y)))

(check-sat)
