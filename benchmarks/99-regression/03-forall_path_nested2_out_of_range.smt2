; Full example of an off-by-one error in forall_path_nested2 function

(set-info :source slplugin)
(set-info :status sat)

(declare-sort LS_t 0)
(declare-sort NLS_t 0)
(declare-heap (Loc Loc)) ; Fix heap decl

(declare-const x NLS_t)

(assert
  (pto x (c_nls nil nil))
)

(assert (not
  (nls nil nil nil)
))

(check-sat)
