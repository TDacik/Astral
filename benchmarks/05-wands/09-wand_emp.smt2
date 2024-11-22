(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert
  (wand
    (pto x (as sep.nil Loc))
    (pto x (as sep.nil Loc))
  )
)

(check-sat)
