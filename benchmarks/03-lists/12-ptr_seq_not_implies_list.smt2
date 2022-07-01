(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)
(declare-const a Loc)
(declare-const b Loc)

(assert
  (sep
    (pto x a)
    (pto a b)
    (pto b y)
  )
)

(assert (not (ls x y)))

(check-sat)
