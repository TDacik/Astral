(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const x DLS_t)
(declare-const y DLS_t)
(declare-const px DLS_t)
(declare-const ny DLS_t)

(assert
  (sep
    (dls x y px ny)
    (pto ny (c_dls px y))
    (pto px (c_dls x ny))
  )
)

(check-sat)
