(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

; Heap cannot be split into two non-empty sub-heap
(assert
  (not
    (sep
      (not sep.emp)
      (not sep.emp)
    )
  )
)

(check-sat)
