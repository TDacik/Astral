(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    (= x nil)
    (pto x y)
  )
)

(check-sat)
