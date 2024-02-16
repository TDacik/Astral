(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x1 Loc)
(declare-const x2 Loc)
(declare-const x3 Loc)
(declare-const x4 Loc)

(assert
  (sep
    (pto x1 x2)
    (pto x2 x3)
    (pto x3 x4)
    (pto x4 x1)
  )
)

(assert (not
  (sep
    (pto x1 x2)
    (pto x2 x3)
    (pto x3 x4)
    (pto x4 x1)
  )
))

(check-sat)
