(set-info :source Astral)
(set-info :status sat)

(include "predicates/nested_singly_linked_list.smt2")

(declare-const x1 Ref_NLS)
(declare-const x2 Ref_NLS)

(declare-const y Ref_LS)

(assert
  (sep
    (pto x1 (c_NLS x2 y))
    (pto x2 (c_NLS nil y))
    (pto y (c_LS nil))
  )
)


(assert (not (nls x1 nil nil)))

(check-sat)
