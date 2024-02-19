(set-info :source Astral)
(set-info :status unsat)

(declare-sort DLS_t 0)
(declare-heap (DLS_t DLS_t))

(declare-const x DLS_t)

(assert (pto x (c_dls nil nil)))
(assert (not (dls x x nil nil)))

(check-sat)
