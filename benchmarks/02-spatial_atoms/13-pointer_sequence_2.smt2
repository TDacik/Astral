(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x1 Loc)
(declare-const x2 Loc)

(assert
  (sep
    (pto x1 x2)
    (pto x2 nil)
  )
)

(check-sat)
