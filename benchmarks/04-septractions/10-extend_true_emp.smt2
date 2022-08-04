(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

; Something can be extended to the empty heap
(assert
  (septraction
    true
    sep.emp
  )
)

(check-sat)
