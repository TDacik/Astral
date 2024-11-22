(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    (ls x y)
    (pto y x)
  )
)

(assert (not
  (sep
    (ls y x)
    (pto x y)
  )
))

(check-sat)
