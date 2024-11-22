; Tests preprocessing of pure constraints containing boolean connectives

(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Int)
(declare-const y Int)

(assert
  (or
    (not (= x x))
    (and
      (= x y)
      (= 0 x)
    )
  )
)

(check-sat)
