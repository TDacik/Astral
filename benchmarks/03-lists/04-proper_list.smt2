(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    (distinct x y)
    (ls x y)
  )
)

(assert (not (pto x y)))

(check-sat)
