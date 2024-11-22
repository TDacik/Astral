(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x Loc)
(declare-const y Loc)
(declare-const i Int)

(assert
  (sep
    (pto x y)
    (= (+ i i) 84)
  )
)


(check-sat)
