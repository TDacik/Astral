(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)

(declare-datatype Val ((c (field Loc))))

(declare-heap (Loc Val))

(define-fun-rec pred ((x Loc) (y Loc)) Bool
  (or
    (and (pto x (c y)) emp)
    emp
  )
)

(declare-const x Loc)
(declare-const y Loc)

(assert (pred x y))

(check-sat)
