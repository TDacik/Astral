(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

; Heap can be split into at lest three parts
(assert
  (sep
    (not sep.emp)
    (not sep.emp)
    (not sep.emp)
  )
)

; Heap cannot be split into four parts
(assert
  (not
    (sep
      (not sep.emp)
      (not sep.emp)
      (not sep.emp)
      (not sep.emp)
    )
  )
)

(check-sat)
