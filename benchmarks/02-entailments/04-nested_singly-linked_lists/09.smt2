(set-info :source Astral)
(set-info :status unsat)

(include "predicates/nested_singly_linked_list.smt2")

(declare-const x1 Ref_NLS)
(declare-const x2 Ref_NLS)
(declare-const x3 Ref_NLS)

(assert
  (sep
    (pto x1 (c_NLS x2 nil))
    (nls x2 x3 nil)
    (pto x3 (c_NLS nil nil))
  )
)


(assert (not (nls x1 nil nil)))

(check-sat)
