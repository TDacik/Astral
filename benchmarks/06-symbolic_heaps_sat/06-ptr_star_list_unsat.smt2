(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    (pto x y)
    (ls x y)
    (distinct x y)
  )
)

(check-sat)
