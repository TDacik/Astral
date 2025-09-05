(set-info :source Astral)
(set-info :status sat)

; def-begin

;; Included from 00-definition.smt2 (modify there and run scripts/include_definitions.py)
; Singly-linked list

(declare-sort Ref_LS 0)

(declare-datatype LS ((c_LS (next Ref_LS))))

(declare-heap (Ref_LS LS))

(define-fun-rec ls ((x Ref_LS) (y Ref_LS)) Bool
  (or
    (= x y)
    (exists ((n Ref_LS))
      (sep
        (distinct x y)
        (pto x (c_LS n))
        (ls n y)
      )
    )
  )
)


; def-end

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert
  (sep
    (distinct x y z)
    (ls x y)
    (ls y z)
  )
)

(assert (not (ls x z)))

(check-sat)
