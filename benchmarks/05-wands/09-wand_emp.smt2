(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert
  (wand
    (pto x (as sep.nil Loc))
    (pto x (as sep.nil Loc))
  )
)

(check-sat)
