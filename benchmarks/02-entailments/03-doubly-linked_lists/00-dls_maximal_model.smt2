(set-info :source Astral)
(set-info :status sat)

(include "predicates/doubly_linked_list.smt2")

(declare-const x Ref_DLS)
(declare-const xp Ref_DLS)

(assert
  (sep
    (distinct x xp nil)
    (dls x nil xp nil)
  )
)

(assert (not
  (sep
    (pto x  (c_DLS xp nil))
    (pto xp (c_DLS nil x))
  )
))

(check-sat)
