(set-info :source Astral)
(set-info :status unsat)

(include "predicates/nested_singly-linked_list.smt2")

(declare-const x1_0 Ref_NLS)
(declare-const x1_1 Ref_LS)
;(declare-const x1_2 Loc)
;(declare-const x1_3 Loc)

;(declare-const x2_0 Loc)
;(declare-const x2_1 Loc)
;(declare-const x2_2 Loc)

(assert
  (sep
    (pto x1_0 (c_NLS nil x1_1))
    (pto x1_1 (c_LS nil))
    ;(pto x1_2 nil)
    ;(pto x1_3 nil)

    ;(pto x2_0 (LocPair nil nil))
    ;(pto x2_1 nil)
    ;(pto x2_2 nil)
  )
)


(assert (not (nls x1_0 nil nil)))

(check-sat)
