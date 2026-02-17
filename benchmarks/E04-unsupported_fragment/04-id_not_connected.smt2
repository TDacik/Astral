(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)

(declare-datatype Val ((c (field Loc) (data Loc))))

(declare-heap (Loc Val))

(define-fun-rec anbn_aux ((x Loc) (y Loc) (b Loc)) Bool
  (pto x (c y b))
)

(define-fun-rec anbn ((x Loc) (y Loc) (a Loc) (b Loc)) Bool
  (or
    (= x y)
    (exists ((n1 Loc) (n2 Loc))
      (sep
        (pto x (c n1 a))
	(anbn n1 n2 a b)
	(anbn_aux n2 y b)
      )
    )
  )
)

(declare-const x Loc)
(declare-const y Loc)
(declare-const a Loc)
(declare-const b Loc)

(assert (anbn x y a b))

(check-sat)
