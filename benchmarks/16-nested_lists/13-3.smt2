(set-info :source Astral)
(set-info :status unsat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

(declare-const x1 NLS_t)
(declare-const x2 NLS_t)
(declare-const x3 NLS_t)

(declare-const x11 LS_t)
(declare-const x12 LS_t)
(declare-const x13 LS_t)

(assert
  (sep
    (pto x1 (c_nls x2 x11))
    (pto x11 x12)
    (pto x12 x13)
    (pto x13 nil)
    (nls x2 x3 nil)
    (pto x3 (c_nls nil nil))
  )
)


(assert (not (nll x1 nil nil)))

(check-sat)
