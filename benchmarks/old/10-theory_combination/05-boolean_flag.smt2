(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)
(declare-const r Bool)

(assert
  (=
    (pto x y)
    r
  )
)

(assert (not r))

(check-sat)
