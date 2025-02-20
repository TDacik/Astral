; Option :use-builtin-definitions is not set

(declare-sort Loc 0)

(set-info :source Astral)
(set-info :status sat)

(declare-const x Loc)
(declare-const y Loc)

(assert (ls x y))

(check-sat)
