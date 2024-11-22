(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x Loc)

(assert
  (sep
    (pto x (as sep.nil Loc))
    (wand
      (pto x (as sep.nil Loc))
      false
    )
  )
)

(check-sat)
