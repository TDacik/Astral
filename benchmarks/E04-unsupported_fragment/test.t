  $ astral 01-id_not_symbolic_heap.smt2
  unknown (Predicate pred: case (pred x y) does not satisfy progress property)

  $ astral 02-id_no_progress.smt2
  unknown (Predicate pred: case (pred x y) does not satisfy progress property)

  $ astral 03-id_outside_sh_fragment.smt2
  unknown (User-defined inductive predicates supported only in the symbolic heap fragment)

  $ astral 04-id_not_connected.smt2
  unknown (Predicate anbn: case (exists (n2 n1) (star (pto x (Val := c(field, data) n1 a)) (anbn n1 n2 a b) (anbn_aux n2 y b))) is not connected)
