(set-info :status unknown)

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert
 (exists ((|e!13| Ref_LS)) (sep (pto x (c_LS |e!13|)) (= x y) (= y z))))