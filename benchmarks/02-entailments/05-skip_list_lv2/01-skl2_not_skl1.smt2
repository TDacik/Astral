(set-info :source Astral)
(set-info :status sat)

(include "predicates/skip_list_lv2.smt2")

(declare-const x Ref_SKL)

(assert
  (sep
    (distinct x nil)
    (skl2 x nil)
  )
)

(assert (not (skl1 x nil)))

(check-sat)
