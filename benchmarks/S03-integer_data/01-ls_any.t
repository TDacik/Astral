  $ astral $NAME
  (define-fun-rec pred1 ((V0 Ref_LS) (V1 Ref_LS)) Bool
   (or (= V1 V0)
    (exists ((E1 (_ BitVec 8)) (E2 Ref_LS))
     (sep (pto V0 (c_LS E2 E1)) (pred1 E2 V1) (distinct V0 V1)))))
  (pred1 x y)
