(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

; Empty heap can be extended to a pointer x -> y
(assert
  (septraction
    sep.emp
    (pto x y)
  )
)

(check-sat)
