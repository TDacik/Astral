  $ astral $NAME
  (define-fun-rec pred1 ((V0 Ref_NLS) (V1 Ref_NLS) (V2 Ref_LS)) Bool
   (or (= V1 V0)
    (exists ((E1 Ref_NLS) (E2 Ref_LS))
     (sep (pto V0 (c_NLS E1 E2)) (pred1 E1 V1 V2) (ls E2 V2) (distinct V1 V0)))))
  (pred1 x y z)
