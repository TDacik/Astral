(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert (pto x y))
(assert (not
  (exists ((z Loc))
    (sep
      (pto x z)
    )
  )
))

(check-sat)
