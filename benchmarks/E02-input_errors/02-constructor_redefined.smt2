; Constructor c is used in two datatypes

(declare-sort Loc 0)

(set-info :source Astral)
(set-info :status sat)

(declare-sort Ref_LS 0)
(declare-sort Ref_NLS 0)

(declare-datatype LS ((c (next Ref_LS))))
(declare-datatype NLS ((c (next Ref_NLS) (down Ref_LS))))


(assert emp)

(check-sat)
