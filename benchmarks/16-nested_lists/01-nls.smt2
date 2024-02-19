(set-info :source Astral)
(set-info :status sat)

(declare-sort LS_t 0)
(declare-sort NLS_t 0)
(declare-heap (Loc Loc))

(declare-const x NLS_t)
(declare-const y NLS_t)

(assert (nls x y nil))

(check-sat)
