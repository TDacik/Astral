(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x1 Loc)
(declare-const x2 Loc)
(declare-const x3 Loc)
(declare-const x4 Loc)

(assert (pto x4 x3))
(assert (pto x2 x4))

(assert (not
  (or
    (sep
      (pto x1 x4)
      (pto x3 x4)
    )
    (pto x2 x2)
    (pto x1 x3)
  )
))

(check-sat)
