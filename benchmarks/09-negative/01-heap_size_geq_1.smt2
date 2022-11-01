(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

; Heap can be split into (at-least) two non-empty sub-heaps
(assert
  (sep
    (not sep.emp)
    (not sep.emp)
  )
)

(check-sat)
