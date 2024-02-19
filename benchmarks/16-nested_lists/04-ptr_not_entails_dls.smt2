(set-info :source Astral)
(set-info :status sat)

(declare-sort NLS_t 0)
(declare-heap (Loc Loc))

(declare-const x1 NLS_t)
(declare-const x2 NLS_t)

(assert (pto x1 (c_nls x2 nil)))

(assert (not (nls x1 x2 nil)))

(check-sat)
