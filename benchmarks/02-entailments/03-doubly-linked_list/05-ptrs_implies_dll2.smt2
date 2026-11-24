(set-info :source Astral)
(set-info :status unsat)

(include "predicates/doubly-linked_list.smt2")

(declare-const x Ref_DLS)
(declare-const y Ref_DLS)
(declare-const z Ref_DLS)

(assert
  (sep
    (pto x (c_DLS y nil))
    (pto y (c_DLS z x))
    (pto z (c_DLS nil y))
  )
)

(assert (not (dls x z nil nil)))

(check-sat)
