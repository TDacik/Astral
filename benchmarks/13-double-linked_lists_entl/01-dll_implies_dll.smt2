(set-info :source Astral)
(set-info :status unsat)

(declare-sort DLS_t 0)
(declare-heap (DLS_t Loc))

(declare-const x DLS_t)
(declare-const y DLS_t)

(assert (dls x y nil nil))
(assert (not (dls x y nil nil)))

(check-sat)
