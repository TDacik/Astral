(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)

(assert
  (sep
    (pto x (as sep.nil Loc))
    (wand
      (pto x (as sep.nil Loc))
      false
    )
  )
)

(check-sat)
