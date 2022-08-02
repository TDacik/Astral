(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(assert
  (sep
    (not emp)
    (not emp)
  )
)

(check-sat)
