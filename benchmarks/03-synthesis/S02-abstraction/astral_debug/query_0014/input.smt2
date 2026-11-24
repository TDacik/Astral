(set-info :status unknown)

(declare-const x Ref_LS)
(declare-const y Ref_LS)
(declare-const z Ref_LS)

(assert
 (exists ((|e!14| Ref_LS)) (sep (pto y (c_LS |e!14|)) (= x y) (= y z))))