(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)
(declare-const z Loc)

(assert
  (sep
    (sep
      (pto z z)
      (pto y x)
    )
    (and
      (pto x z)
      (not (pto y y))
    )
  )
)

(check-sat)
