; Bug reported in https://github.com/cvc5/cvc5/issues/8863

(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)
(declare-const z Loc)

(assert
  (sep
    (septraction
      sep.emp
      (pto x z)
    )
    (distinct y z)
    (distinct y x)
    (distinct z x)
  )
)

(assert
  (pto x y)
)

(check-sat)
