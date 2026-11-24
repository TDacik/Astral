(set-info :source Astral)
(set-info :status sat)

(include "predicates/nested_singly-linked_list.smt2")

(declare-const x1_1 Ref_NLS)
(declare-const x2_1 Ref_NLS)
(declare-const x3_1 Ref_NLS)

(declare-const x1_2 Ref_LS)
(declare-const x2_2 Ref_LS)

(declare-const x1_3 Ref_LS)

(assert
  (sep
    (distinct x1_1 x3_1)
    (pto x1_1 (c_NLS x2_1 x1_2))
    (pto x2_1 (c_NLS x3_1 x2_2))
    (pto x1_2 (c_LS x1_3))
    (pto x2_2 (c_LS nil))
    (pto x1_3 (c_LS nil))
  )
)


(assert (not (nls x1_1 x3_1 nil)))

(check-sat)
