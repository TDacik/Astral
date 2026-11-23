(set-info :source Astral)
(set-info :status unsat)

(include "predicates/singly_linked_list.smt2")

(declare-const x1 Ref_LS)
(declare-const x2 Ref_LS)
(declare-const x3 Ref_LS)
(declare-const x4 Ref_LS)

(assert
  (sep
    (pto x1 x2)
    (pto x2 x3)
    (pto x3 x4)
    (pto x4 x1)
  )
)

(assert (not
  (sep
    (pto x1 x2)
    (pto x2 x3)
    (pto x3 x4)
    (pto x4 x1)
  )
))

(check-sat)
