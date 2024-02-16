(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)

(assert
  (sep
    (pto x x)
    (pto x nil)
  )
)

(check-sat)
