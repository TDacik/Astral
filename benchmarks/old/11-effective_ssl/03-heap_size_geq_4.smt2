(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

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
