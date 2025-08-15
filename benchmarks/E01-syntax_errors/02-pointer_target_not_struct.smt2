(set-info :source Astral)
(set-info :status sat)

(declare-sort Ref_T 0)

(declare-datatype T ((c_LS (next1 Ref_T) (next2 Ref_T))))

(declare-heap (Ref_T T))

(declare-const x Ref_T)
(declare-const y Ref_T)

(assert
  (pto x y y)
)

(check-sat)
