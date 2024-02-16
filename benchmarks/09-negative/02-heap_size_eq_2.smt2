(set-info :source Astral)
(set-info :status sat)

(declare-sort Loc 0)
(declare-heap (Loc Loc))

; Heap can be split into two non-empty sub-heaps
;   => its size at least 2
(assert
  (sep
    (not sep.emp)
    (not sep.emp)
  )
)

; Heap cannot be split into three non-empty sub-heaps
;   => its size at most 2
(assert
  (not
    (sep
      (not sep.emp)
      (not sep.emp)
      (not sep.emp)
    )
  )
)

(check-sat)
