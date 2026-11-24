(set-info :status unknown)

(declare-const x Ref_LS)
(declare-const y Ref_LS)

(assert
 (or (= x y)
  (exists ((n Ref_LS)) (sep (distinct x y) (pto x (c_LS n)) (ls n y)))))