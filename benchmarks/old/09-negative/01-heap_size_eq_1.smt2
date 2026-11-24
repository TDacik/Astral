(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

; Heap is not empty
;   => its size at least 1
(assert (not emp))

; Heap cannot be split into two non-empty sub-heaps
;   => its size at most 1
(assert
  (not
    (sep
      (not sep.emp)
      (not sep.emp)
    )
  )
)

(check-sat)
