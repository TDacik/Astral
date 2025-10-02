(set-info :source Astral)
(set-info :status unsat)

; def-begin
; def-end

; The following model is a valid DLS according to standard semantics
; used SL-comp:
;
;    _______
;   |      v
;   `---[x, yp] <-> xp -> y
;

(declare-const x Ref_DLS)
(declare-const y Ref_DLS)
(declare-const xp Ref_DLS)
(declare-const yp Ref_DLS)

(assert
  (sep
    (pto x (c_DLS xp yp))
    (pto xp (c_DLS y x))
    (= x yp)
    (distinct x xp y)
  )
)

(assert (not (dls x y xp yp)))

(check-sat)
