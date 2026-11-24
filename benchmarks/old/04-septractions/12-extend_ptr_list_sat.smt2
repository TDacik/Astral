(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)
(declare-const z Loc)

(assert
  (septraction
    (pto x y)
    (ls x z)
  )
)

(check-sat)
