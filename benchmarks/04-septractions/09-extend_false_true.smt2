(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

; False can be extended
(assert
  (septraction
    false
    true
  )
)

(check-sat)
