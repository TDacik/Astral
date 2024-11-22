(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)
(declare-const a Loc)
(declare-const b Loc)

(assert
  (sep
    (pto x a)
    (pto a b)
    (pto b y)
  )
)

(assert (not (ls x y)))

(check-sat)
