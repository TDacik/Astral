(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

; Empty heap can be extended to a non-empty heap
(assert
  (septraction
    sep.emp
    (not sep.emp)
  )
)

(check-sat)
