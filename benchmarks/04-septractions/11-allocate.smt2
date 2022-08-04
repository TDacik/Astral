(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

; x is not allocated
(assert
  (septraction
    (pto x nil)
    true
  )
)

(check-sat)
