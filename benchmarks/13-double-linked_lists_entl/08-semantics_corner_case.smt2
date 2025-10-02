(set-info :source Astral)
(set-info :status sat)

(set-option :use-builtin-definitions)

; The following model is a valid DLS according to standard semantics
; used SL-comp. However, our built-in semantics does not allow it.
;
;    _______
;   |      v
;   `---[x, yp] <-> xp -> y
;


(declare-const x DLS_t)
(declare-const y DLS_t)
(declare-const xp DLS_t)
(declare-const yp DLS_t)

(assert
  (sep
    (pto x (c_dls xp yp))
    (pto xp (c_dls y x))
    (= x yp)
    (distinct x xp y)
  )
)

(assert (not (dls x xp yp y)))

(check-sat)
