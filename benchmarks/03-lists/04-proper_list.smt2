(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    (distinct x y)
    (ls x y)
  )
)

(assert (not (pto x y)))

(check-sat)
