(set-info :source Astral)
(set-info :status unsat)

(include "predicates/singly_linked_list.smt2")

(declare-const x Ref_LS)

(assert
  (sep
    (distinct x nil)
    (ls nil x)
  )
)

(check-sat)
