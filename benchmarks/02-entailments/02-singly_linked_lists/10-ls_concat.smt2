(set-info :source Astral)
(set-info :status unsat)

(include "predicates/singly-linked_list.smt2")

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert
  (sep
    (ls x y)
    (ls y z)
    (ls z nil)
  )
)

(assert (not (ls x nil)))

(check-sat)
