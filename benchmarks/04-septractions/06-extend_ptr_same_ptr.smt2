(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

; Pointer x -> y can be extended to pointer x -> y
(assert
  (septraction
    (pto x y)
    (pto x y)
  )
)

(check-sat)
