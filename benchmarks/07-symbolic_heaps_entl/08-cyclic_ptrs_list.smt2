(set-info :source Astral)
(set-info :status unsat)

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
    (distinct x z)
    (distinct y z)
  )
)

(assert (not (ls x z)))

(check-sat)
