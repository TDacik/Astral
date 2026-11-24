(set-info :status unknown)

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert
 (exists ((|e!15| Ref_LS)) (sep (pto z (c_LS |e!15|)) (= x y) (= y z))))