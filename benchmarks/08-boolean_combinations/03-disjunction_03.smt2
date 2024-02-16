(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert (pto x y))

(assert (not
  (sep
    (or
      (pto x x)
      (pto y y)
    )
    sep.emp
  )
))

(check-sat)
