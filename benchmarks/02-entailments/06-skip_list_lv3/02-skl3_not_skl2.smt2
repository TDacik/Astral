(set-info :source Astral)
(set-info :status sat)

(include "predicates/skip_list_lv3.smt2")

(declare-const x Ref_SKL)

(assert
  (sep
    (distinct x nil)
    (skl3 x nil)
  )
)

(assert (not (skl2 x nil)))

(check-sat)
