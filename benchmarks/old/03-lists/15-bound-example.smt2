(set-info :source Astral)
(set-info :status unsat)

(include "predicates/singly_linked_list.smt2")

(declare-const a Ref_LS)
(declare-const b Ref_LS)
(declare-const c Ref_LS)
(declare-const d Ref_LS)

(assert
  (sep
    (ls a b)
    (pto b c)
    (pto c d)
    (ls d a)
  )
)

(assert (not
  (sep
    (ls a c)
    (ls c a)
  )
))

(check-sat)
