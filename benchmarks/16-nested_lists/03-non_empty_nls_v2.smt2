(set-info :source Astral)
(set-info :status sat)

(declare-sort NLS_t 0)
(declare-heap (Loc Loc))

(declare-const x NLS_t)
(declare-const y NLS_t)

(assert
  (sep
    (distinct x y nil)
    (nls x y nil)
  )
)

(check-sat)
