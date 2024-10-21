(set-logic QF_ALL)
(set-option :status unknown)

(declare-sort LS_t 0)
(declare-heap (LS_t LS_t))

(declare-const s LS_t)
(declare-const start LS_t)

(assert
  (sep
    (and
      (sep
        (distinct start nil)
        (ls start nil)
      )
      (not
        (pto start nil)
      )
    )
    (distinct start nil)
    (= nil s)
  )
)

(assert (not
  (exists ((s!40 LS_t) (s!41 LS_t))
    (or
      (sep
        (pto s!40 nil)
        (pto start s!40)
        (distinct s!40 nil)
        (distinct start nil)
        (= start s)
      )
      (sep
        (pto s nil)
        (pto start s)
        (distinct s nil)
        (distinct start nil)
      )
      (sep
        (and
          (sep
            (distinct start s!41)
            (ls start s!41)
          )
          (not (pto start s!41))
        )
        (pto s!41 nil)
        (distinct s!41 nil)
        (distinct start nil)
        (= start s)
      )
    )
  )
))

(check-sat)
