(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    (pto x y)
    (pto y nil)
  )
)

(assert (not (ls x nil)))

(check-sat)
