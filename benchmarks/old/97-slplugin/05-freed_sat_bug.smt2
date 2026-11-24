(set-info :source ktsn)
(set-info :status sat)

(set-option :use-freed-predicate)

(declare-sort Loc 0)

(declare-datatype LS_t ((c_ls (field_next Loc))))

(declare-heap (Loc LS_t))

(define-fun-rec ls ((x Loc) (y Loc)) Bool
  (or
    (= x y)
    (exists ((e Loc))
      (sep (distinct x y) (pto x (c_ls e)) (ls e y))
    )
  )
)


(declare-const node Loc)
(declare-const s Loc)

(assert
  (sep
    (= s node)
    (freed s)
    (ls node s)
  )
)
