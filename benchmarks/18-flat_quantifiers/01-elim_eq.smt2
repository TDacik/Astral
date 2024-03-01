(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert (ls x y))
(assert (not
  (exists ((z Loc))
    (sep
      (pto x z)
      (= z y)
    )
  )
))

(check-sat)
