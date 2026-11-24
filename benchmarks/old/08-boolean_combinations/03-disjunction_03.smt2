(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert (pto x y))

(assert (not
  (sep
    (or
      (pto x x)
      (pto y y)
    )
    sep.emp
  )
))

(check-sat)
