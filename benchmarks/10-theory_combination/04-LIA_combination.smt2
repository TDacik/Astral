(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

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
