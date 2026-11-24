(set-info :source Astral)
(set-info :status unsat)

; def-begin
; def-end


(declare-const x Ref_TREE)
(declare-const h1 Ref_TREE)
(declare-const h2 Ref_TREE)
(declare-const h3 Ref_TREE)
(declare-const h4 Ref_TREE)
(declare-const h5 Ref_TREE)
(declare-const h6 Ref_TREE)
(declare-const h7 Ref_TREE)
(declare-const h8 Ref_TREE)
(declare-const h9 Ref_TREE)

(assert
  (sep
    (tree x h1)
    (tree h1 h2)
    (tree h2 h3)
    (tree h3 h4)
    (tree h4 h5)
    (tree h5 h6)
    (tree h6 h7)
    (tree h7 h8)
    (tree h8 h9)
    (tree h9 nil)
  )
)

(assert (not (tree x nil)))

(check-sat)
