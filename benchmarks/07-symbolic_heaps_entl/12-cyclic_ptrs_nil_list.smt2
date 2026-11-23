(set-info :source Astral)
(set-info :status unsat)

(include "predicates/singly_linked_list.smt2")

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert
  (sep
    (pto x y)
    (pto y nil)
  )
)

(assert (not (ls x nil)))

(check-sat)
