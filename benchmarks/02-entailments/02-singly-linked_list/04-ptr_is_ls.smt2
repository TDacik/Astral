(set-info :source Astral)
(set-info :status unsat)

(include "predicates/singly-linked_list.smt2")

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert
  (sep
    (pto x (c_LS y))
    (distinct x y)
  )
)

(assert (not (ls x y)))

(check-sat)
