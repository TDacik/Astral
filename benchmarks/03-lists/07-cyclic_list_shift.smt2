(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    (ls x y)
    (pto y x)
  )
)

(assert (not
  (sep
    (ls y x)
    (pto x y)
  )
))

(check-sat)
