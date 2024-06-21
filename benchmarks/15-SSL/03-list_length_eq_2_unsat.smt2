(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

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
