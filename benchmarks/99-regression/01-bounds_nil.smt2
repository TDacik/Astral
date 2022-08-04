(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)
(declare-const z Loc)

(assert
  (sep
    (sep
      (pto z z)
      (pto y x)
    )
    (and
      (pto x z)
      (not (pto y y))
    )
  )
)

(check-sat)
