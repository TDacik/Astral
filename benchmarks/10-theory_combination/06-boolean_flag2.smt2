(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)
(declare-const r Bool)

(assert
  (=
    (= x y)
    r
  )
)

(assert
  (=
    (= x y)
    (not r)
  )
)

(check-sat)
