(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert (ls x y))

(assert
  (sep
    (not sep.emp)
    (not sep.emp)
  )
)

(assert (not
  (sep
    (not sep.emp)
    (not sep.emp)
    (not sep.emp)
  )
))

(check-sat)
