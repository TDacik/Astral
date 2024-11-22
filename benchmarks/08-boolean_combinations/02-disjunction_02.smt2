(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert (pto x y))

(assert (not
  (sep
    (or
      (pto x y)
      sep.emp
    )
    sep.emp
  )
))

(check-sat)
