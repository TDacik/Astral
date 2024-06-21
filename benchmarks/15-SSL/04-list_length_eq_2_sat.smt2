(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)
(declare-const z Loc)

(assert (ls x y))

(assert
  (sep
    (not sep.emp)
    (not sep.emp)
  )
)

(assert (not
  (sep
    (not sep.emp)
    (not sep.emp)
    (not sep.emp)
  )
))

(check-sat)
