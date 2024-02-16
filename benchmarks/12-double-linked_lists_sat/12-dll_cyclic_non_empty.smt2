(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)
(declare-const f Loc)
(declare-const l Loc)

(assert
  (sep
    (distinct x l)
    (dls x y f l)
    (pto l (locPair f y))
    (pto f (locPair x l))
  )
)

(check-sat)
