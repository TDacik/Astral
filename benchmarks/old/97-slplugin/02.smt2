; Full example of an off-by-one error in forall_path_nested2 function

(set-info :source slplugin)
(set-info :status unsat)

(set-option :use-builtin-definitions)

(declare-const s_4 NLS_t)
(declare-const s NLS_t)
(declare-const start NLS_t)

(assert
  (sep
    (= nil s_4)
    (= start s)
    (distinct s nil)
    (pto s (c_nls nil nil))
  )
)

(assert (not
  (or
    (sep
      (= start s)
      (distinct s nil)
      (pto s (c_nls s_4 nil))
      (sep
        (nls s_4 nil nil)
        (distinct s_4 nil)
      )
    )

    (sep
      (= nil s_4)
      (= start s)
      (distinct s nil)
      (pto s (c_nls nil nil))
    )

    (sep
      (= nil s_4)
      (distinct start nil)
      (pto start (c_nls s nil))
      (pto s (c_nls nil nil))
    )

    (sep
      (= start s)
      (distinct s nil)
      (pto s (c_nls s_4 nil))
      (sep
        (nls s_4 nil nil)
        (distinct s_4 nil)
      )
    )

    (sep
      (distinct start nil)
      (pto start (c_nls s nil))
      (pto s (c_nls s_4 nil))
      (sep
        (nls s_4 nil nil)
        (distinct s_4 nil)
      )
    )
  )
))

(check-sat)
