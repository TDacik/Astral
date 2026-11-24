  $ astral 02-frame_problem.smt2
  (define-fun-rec pred1 ((V0 Ref_LS) (V1 Ref_LS)) Bool
   (or (= V1 V0)
    (exists ((E1 Ref_LS))
     (sep (pto V0 (c_LS E1)) (pred1 E1 V1) (distinct V1 V0)))))
  (pred1 x y)
