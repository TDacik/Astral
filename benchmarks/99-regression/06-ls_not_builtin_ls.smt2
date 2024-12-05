; User-defined predicate is defined with the same name as some
; built-in predicate (and :use-builtin-definitions is not set)

(set-info :source Astral)
(set-info :status sat)

(declare-sort LS_t 0)

(declare-datatype LS ((c_LS (next LS_t) (next2 LS_t))))

(declare-heap (LS_t LS))

(define-fun-rec ls ((x LS_t) (y LS_t)) Bool
  (or
    (= x y)
    (exists ((n LS_t))
      (sep
        (distinct x y)
        (pto x (c_LS n n))
        (ls n y)
      )
    )
  )
)

(declare-const x LS_t)
(declare-const y LS_t)

(assert (ls x y))

(check-sat)
