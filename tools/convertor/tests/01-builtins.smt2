(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)
(declare-const z Loc)

(assert
  (sep
    (= x nil)
    (distinct y nil)
    (ls x y)
    (pto y x)
  )
)

(assert (not
  (sep
    (distinct x y z)
    (ls y x)
    (pto x y)
  )
))

(check-sat)
