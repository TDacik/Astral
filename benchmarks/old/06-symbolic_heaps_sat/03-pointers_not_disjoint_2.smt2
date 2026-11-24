(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x Loc)

(assert
  (sep
    (pto x x)
    (pto x nil)
  )
)

(check-sat)
