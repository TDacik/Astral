(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)
(declare-const r Bool)

(assert
  (=
    (pto x y)
    r
  )
)

(assert (not r))

(check-sat)
