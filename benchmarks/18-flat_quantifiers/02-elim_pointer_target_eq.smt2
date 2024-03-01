(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert (pto x y))
(assert (not
  (exists ((z Loc))
    (sep
      (pto x z)
    )
  )
))

(check-sat)
