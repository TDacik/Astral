(set-info :source Astral)
(set-info :status unsat)

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
