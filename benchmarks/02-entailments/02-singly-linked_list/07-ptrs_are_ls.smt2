(set-info :source Astral)
(set-info :status unsat)

(include "predicates/singly-linked_list.smt2")

(declare-const x1 Ref_LS)
(declare-const x2 Ref_LS)
(declare-const x3 Ref_LS)
(declare-const x4 Ref_LS)

(assert
  (sep
    (distinct x1 x4)
    (distinct x2 x4)
    (distinct x3 x4)
    (pto x1 x2)
    (pto x2 x3)
    (pto x3 x4)
  )
)

(assert (not (ls x1 x4)))

(check-sat)
