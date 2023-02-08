(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)
(declare-const z Loc)

(assert
  (sep
	  (pto x (locPair y nil))
		(pto y (locPair z x))
		(pto z (locPair nil y))
  )
)

(assert (not (dls x z nil nil)))

(check-sat)
