(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

; Empty heap cannot be extended
(assert
  (septraction
    sep.emp
    false
  )
)

(check-sat)
