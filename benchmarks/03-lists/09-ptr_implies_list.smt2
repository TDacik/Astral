(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    (distinct x y)
    (pto x y)
  )
)

(assert (not (ls x y)))

(check-sat)
