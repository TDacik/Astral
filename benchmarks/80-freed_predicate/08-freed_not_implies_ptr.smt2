(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert
  (freed x)
)

(assert (not
  (exists ((z Loc))
    (pto x z)
  )
))

(check-sat)
