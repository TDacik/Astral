(set-info :source slplugin)
(set-info :status sat)

(declare-sort LS_t 0)
(declare-sort NLS_t 0)
(declare-heap (Loc Loc)) ; Fix heap decl

(assert (nls nil nil nil))

(check-sat)
