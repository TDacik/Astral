(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert (ls x y))
(assert (not
  (exists ((z Loc))
    (sep
      (pto x z)
      (= z y)
    )
  )
))

(check-sat)
