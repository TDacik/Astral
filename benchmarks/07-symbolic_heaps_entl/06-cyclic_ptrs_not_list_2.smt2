(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)
(declare-const z Loc)

(assert
  (sep
    (pto x y)
    (pto y z)
    (distinct x y)
  )
)

(assert (not (ls x z)))

(check-sat)
