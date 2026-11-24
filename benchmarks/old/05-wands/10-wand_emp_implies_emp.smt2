(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert (not
  (wand
    (pto x (as sep.nil Loc))
    (pto x (as sep.nil Loc))
  )
))

(assert (not sep.emp))

(check-sat)
