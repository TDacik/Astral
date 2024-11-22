(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)
(declare-const z Loc)

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
