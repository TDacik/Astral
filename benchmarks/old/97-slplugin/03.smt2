(set-info :status sat)

(set-option :use-builtin-definitions)

(declare-const A_s_5 Loc)
(declare-const node_6_11 Loc)
(declare-const node_6 Loc)
(declare-const s_5 Loc)


(assert
 (sep (= node_6 s_5 A_s_5) (distinct s_5 nil) (pto node_6_11 (c_ls node_6))
  (and (sep (ls s_5 node_6_11) (distinct s_5 node_6_11))
   (not (pto s_5 (c_ls node_6_11))))
  (ls node_6 s_5)))
