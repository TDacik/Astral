(set-info :source Astral)
(set-info :status sat)

(include "predicates/doubly_linked_list.smt2")

(declare-const x Ref_DLS)
(declare-const y Ref_DLS)

(assert
  (sep
    (distinct x y)
    (distinct x nil)
    (dls x y nil nil)
  )
)

(assert (not
  (sep
    (pto x (c_DLS y nil))
    (pto y (c_DLS nil x))
  )
))

(check-sat)
