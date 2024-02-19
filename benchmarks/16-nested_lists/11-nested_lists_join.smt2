(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x1 NLS_t)
(declare-const x2 NLS_t)

(declare-const y LS_t)

(assert
  (sep
    (pto x1 (c_nls x2 y))
    (pto x2 (c_nls nil y))
    (pto y nil)
  )
)


(assert (not (nll x1 nil nil)))

(check-sat)
