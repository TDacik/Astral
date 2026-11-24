  $ astral 05-predicate_name_exists.smt2
  (define-fun-rec pred2 ((V0 Ref_NLS) (V1 Ref_NLS) (V2 Ref_LS)) Bool
   (or (= V1 V0)
    (exists ((E1 Ref_LS) (E2 Ref_NLS))
     (sep (pto V0 (c_NLS E2 E1)) (pred2 E2 V1 V2) (pred1 E1 V2)
      (distinct V0 V1)))))
  (pred2 x y z)
