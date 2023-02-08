(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert (not
  (wand
    (pto x (as sep.nil Loc))
    (pto x (as sep.nil Loc))
  )
))

(assert (not sep.emp))

(check-sat)
