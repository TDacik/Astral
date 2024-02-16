(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert (not
  (sep
    (or
      sep.emp
      sep.emp
    )
    sep.emp
  )
))

(check-sat)
