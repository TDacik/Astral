(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y1 Loc)
(declare-const y2 Loc)

(assert
  (sep
    (distinct y1 y2)
    (ls x y1)
    (ls x y2)
  )
)

(check-sat)
