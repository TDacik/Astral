(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

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
