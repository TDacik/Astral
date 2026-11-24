; Full example of an off-by-one error in forall_path_nested2 function

(set-info :source slplugin)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x NLS_t)

(assert
  (pto x (c_nls nil nil))
)

(assert (not
  (nls nil nil nil)
))

(check-sat)
