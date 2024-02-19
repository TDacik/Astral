(set-info :source Astral)
(set-info :status sat)

(declare-sort DLS_t 0)
(declare-heap (DLS_t DLS_t))

(declare-const x DLS_t)
(declare-const y DLS_t)

(assert (pto x (c_dls nil nil)))
(assert (not (dls x y nil nil)))

(check-sat)
