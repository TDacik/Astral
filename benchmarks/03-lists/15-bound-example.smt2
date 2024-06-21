(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const a Loc)
(declare-const b Loc)
(declare-const c Loc)
(declare-const d Loc)

(assert
  (sep
    (ls a b)
    (pto b c)
    (pto c d)
    (ls d a)
  )
)

(assert (not
  (sep
    (ls a c)
    (ls c a)
  )
))

(check-sat)
