(set-info :source Astral)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    (freed x)
    (freed y)
    (= x y)
  )
)

(check-sat)
