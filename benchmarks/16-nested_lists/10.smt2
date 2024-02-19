(set-info :source Astral)
(set-info :status unsat)

(declare-sort NLS_t 0)
(declare-heap (NLS_t NLS_t))

(declare-const x1 NLS_t)
(declare-const x1_1 LS_t)

(assert
  (sep
    (pto x1 (c_nls nil x1_1))
    (distinct x1_1 nil)
    (ls x1_1 nil)
  )
)


(assert (not (nll x1 nil nil)))

(check-sat)
