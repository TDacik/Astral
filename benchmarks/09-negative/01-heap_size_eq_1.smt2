(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

; Heap is not empty
(assert (not emp))

; Heap cannot be split into two non-empty sub-heaps
(assert
  (not
    (sep
      (not sep.emp)
      (not sep.emp)
    )
  )
)

(check-sat)
