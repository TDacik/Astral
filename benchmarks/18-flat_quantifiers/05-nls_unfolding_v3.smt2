(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x NLS_t)

(assert
  (and
    (sep
      (nls x nil nil)
      (distinct x nil)
    )
    (not
      (pto x (c_nls nil nil))
    )
  )
)

(assert (not
  (or
    (exists ((n Loc))
      (sep
        (pto x (c_nls nil n))
        (ls n nil)
      )
    )
    (exists ((t NLS_t))
      (sep
        (pto x (c_nls t nil))
        (pto t (c_nls nil nil))
      )
    )
  )
))

(check-sat)
