(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    (distinct x y)
    (distinct x nil)
    (dls x y nil nil)
  )
)

(assert (not
  (sep
    (pto x (locPair y nil))
    (pto y (locPair nil x))
  )
))

(check-sat)
