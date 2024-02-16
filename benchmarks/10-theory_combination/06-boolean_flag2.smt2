(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)
(declare-const r Bool)

(assert
  (=
    (= x y)
    r
  )
)

(assert
  (=
    (= x y)
    (not r)
  )
)

(check-sat)
