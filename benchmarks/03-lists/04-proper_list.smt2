(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)

(assert
  (sep
    (distinct x nil)
    (ls x nil)
  )
)

(assert (not (pto x nil)))

(check-sat)
