; Bug reported in https://github.com/cvc5/cvc5/issues/8659
; (modified to the precise semantics of pure atoms and
;  rewritten using septraction)

(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x Loc)
(declare-const y Loc)

(assert
  (sep
    (septraction
      (pto x x)
      (pto x x)
    )
    (pto x y)
    (distinct x y)
  )
)

(check-sat)
