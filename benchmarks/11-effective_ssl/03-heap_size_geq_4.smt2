(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

; Heap can be split into four non-empty sub-heaps
;   => its size at least 4
(assert
  (sep
    (not sep.emp)
    (not sep.emp)
    (not sep.emp)
  )
)

(check-sat)
