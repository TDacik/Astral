(set-info :source Astral)
(set-info :status sat)

(include "predicates/doubly-linked_list.smt2")

(declare-const x Ref_DLS)

(assert
  (sep
    (pto x (c_DLS y x))
    (pto y (c_DLS y x))
  )
)

(assert (not (dls x y x y)))

(check-sat)
