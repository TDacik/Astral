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

(declare-const x1 Ref_LS)
(declare-const y1 Ref_LS)
(declare-const z1 Ref_LS)

(declare-const x2 Ref_LS)
(declare-const y2 Ref_LS)

(assert
  (sep
    (ls x1 y1)
    (ls y1 z1)

    (ls x2 y2)
    (ls y2 nil)

  )
)

(assert (not
  (sep
    (ls x1 z1)
    (ls x2 nil)
  )
))

(check-sat)
