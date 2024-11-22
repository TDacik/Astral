(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    (= x y)
    (ls x y)
  )
)

(assert (not emp))

(check-sat)
