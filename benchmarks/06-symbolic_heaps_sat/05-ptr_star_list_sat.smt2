(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    (pto x y)
    (ls x y)
  )
)

(check-sat)
