(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)

(assert 
  (sep
    (distinct x nil)
    (ls nil x)
  )
)

(check-sat)
