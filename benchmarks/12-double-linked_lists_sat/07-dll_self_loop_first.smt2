(set-info :source Astral)
(set-info :status sat)

(declare-sort DLS_t 0)
(declare-heap (DLS_t DLS_t))

(declare-const x DLS_t)
(declare-const y DLS_t)

(assert (dls x y x nil))

(check-sat)
