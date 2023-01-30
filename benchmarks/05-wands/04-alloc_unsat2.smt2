(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    (pto x y)
    (wand
      (pto x y)
      false
    )
  )
)

(check-sat)
