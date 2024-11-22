(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert (not
  (sep
    (or
      sep.emp
      sep.emp
    )
    sep.emp
  )
))

(check-sat)
