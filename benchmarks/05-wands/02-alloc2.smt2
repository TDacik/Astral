(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert
  (wand
    (pto x (as sep.nil Loc))
    false
  )
)

(assert
  (wand
    (pto y (as sep.nil Loc))
    false
  )
)

(check-sat)

; Tests
(set-info :alloc x)
(set-info :alloc y)
