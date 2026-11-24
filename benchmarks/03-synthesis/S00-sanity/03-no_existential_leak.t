  $ astral $NAME
  (define-fun-rec pred1 ((V0 Ref_NLS) (V1 Ref_NLS)) Bool
   (or (= V1 V0)
    (exists ((E1 Ref_LS) (E2 Ref_NLS))
     (sep (pto V0 (c_NLS E2 E1)) (pred1 E2 V1) (ls E1 nil) (distinct V1 V0)))))
  (pred1 x y)
