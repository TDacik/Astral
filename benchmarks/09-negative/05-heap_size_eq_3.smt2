(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

; Heap can be split into (at lest) three non-empty sub-heaps
(assert
  (sep
    (not sep.emp)
    (not sep.emp)
    (not sep.emp)
  )
)

; Heap cannot be split into four non-empty sub-heaps
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
