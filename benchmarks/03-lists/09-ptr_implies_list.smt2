(set-info :source Astral)
(set-info :status unsat)

(include "predicates/singly_linked_list.smt2")

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert
  (sep
    (distinct x y)
    (pto x y)
  )
)

(assert (not (ls x y)))

(check-sat)
