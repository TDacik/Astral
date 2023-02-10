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
    (distinct y f)
    (dls x y f l)
  )
)

(check-sat)
