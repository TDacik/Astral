(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)
(declare-const z Loc)

(assert
  (sep
    (pto x y)
    (pto y z)
    (distinct y z)
    (distinct x z)
  )
)

(assert (not (ls x z)))

(check-sat)
