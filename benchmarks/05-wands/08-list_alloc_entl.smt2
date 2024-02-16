(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert (ls x y))

(assert
  (wand
    (pto x (as sep.nil Loc))
    false
  )
)

; Entails not emp
(assert (not (not emp)))

(check-sat)
