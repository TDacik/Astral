(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
	  (pto x (locPair y x))
		(pto y (locPair y x))
  )
)

(assert (not (dls x y x y)))

(check-sat)
