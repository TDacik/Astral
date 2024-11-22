(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y1 Loc)
(declare-const y2 Loc)

(assert
  (sep
    (distinct y1 y2)
    (ls x y1)
    (ls x y2)
  )
)

(check-sat)
