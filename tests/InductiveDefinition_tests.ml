(* Tests for operations over inductive definitions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open InductiveDefinition_testable

module SL = SL_testable
open SL

let sid_ls = InductiveDefinition.ID_map.of_list [("ls", ls)]

let instantiate_test1 () =
  let actual = InductiveDefinition.instantiate ~refresh:false ls [u; v] in
  let expected = ls_def u v in
  SL.check_equal actual expected

let instantiate_test2 () =
  let actual = InductiveDefinition.instantiate ~refresh:true ls [u; v] in
  let expected = ls_def u v in
  SL.check_equal actual expected


(** Unfolding tests *)

(* TODO: we could test more properties of unfolded formulas *)

let unfold_test1 () =
  let actual = InductiveDefinition.unfold sid_ls ls [x; y] 0 in
  let expected = SL.mk_eq [x; y] in
  SL.check_equal actual expected

let unfold_test2 () =
  let actual = InductiveDefinition.unfold sid_ls ls [x; y] 1 in
  let expected =
    SL.mk_or [
      SL.mk_eq [x; y];
      SL.mk_exists' [SL_builtins.loc_ls] (fun [n] ->
        SL.mk_star [
          SL.mk_distinct [x; y];
          SL.mk_pto x n;
          SL.mk_eq [n; y]
  ])]
  in
  SL.check_equal actual expected

let unfold_test3 () =
  let actual = InductiveDefinition.unfold sid_ls ls [x; y] 2 in
  let expected =
    SL.mk_or [
      SL.mk_eq [x; y];
      SL.mk_exists' [SL_builtins.loc_ls] (fun [n] ->
        SL.mk_star [
          SL.mk_distinct [x; y];
          SL.mk_pto x n;

            SL.mk_or [
              SL.mk_eq [n; y];
              SL.mk_exists' [SL_builtins.loc_ls] (fun [n2] ->
                SL.mk_star [
                  SL.mk_distinct [n; y];
                  SL.mk_pto n n2;
                  SL.mk_eq [n2; y];
            ])]
  ])]
  in
  SL.check_equal actual expected

let unfold_test4 () =
  let id = InductiveDefinition.map IntroduceIfThenElse.apply ls in
  let sid = InductiveDefinition.ID_map.of_list [("ls", id)] in
  let actual = InductiveDefinition.unfold sid id [x; y] 0 in
  let expected = SL.mk_eq [x; y] in
  SL.check_equal actual expected

let unfold_test5 () =
  let id = InductiveDefinition.map IntroduceIfThenElse.apply ls in
  let sid = InductiveDefinition.ID_map.of_list [("ls", id)] in
  let actual = InductiveDefinition.unfold sid id [x; y] 1 in
  let expected =
    SL.mk_ite
      (SL.mk_eq [x; y])
      (SL.emp)
      (SL.mk_exists' [SL_builtins.loc_ls] (fun [n] ->
        SL.mk_star [
          SL.mk_pto x n;
          SL.mk_eq [n; y]
        ]))
  in
  SL.check_equal actual expected

let unfold_tll_test1 () =
  let open TLL in
  let sid = InductiveDefinition.ID_map.of_list [("tll", id)] in
  let actual = Simplifier.simplify @@ InductiveDefinition.unfold sid id [x; y; z] 3 in
  let expected =
    SL.mk_or [
      SL.mk_star [
        SL.mk_pto_struct x def [z; nil; nil];
        SL.mk_eq [x; y]
      ];
      SL.mk_exists' [sort; sort; sort] (fun [l; r; mid] ->
        SL.mk_star [
          SL.mk_pto_struct x def [l; r; nil];
          SL.mk_pto_struct l def [mid; nil; nil];
          SL.mk_pto_struct r def [z; nil; nil];
          SL.mk_eq [l; y];
          SL.mk_eq [r; mid];
        ]
      )
    ]
  in
  SL.check_equal actual expected

(** Guided unfolding tests *)

let guided_unfold_test1 () =
  let id = InductiveDefinition.map IntroduceIfThenElse.apply ls in
  let sid = InductiveDefinition.ID_map.of_list [("ls", id)] in
  let g = SL_graph.compute (SL.mk_eq [x; y]) in
  let actual = InductiveDefinition.unfold_guided sid id g [x; y] 10 in
  let expected = SL.emp in
  SL.check_equal actual expected

let guided_unfold_test2 () =
  SID.register_user_defined ls; (* TODO: why? *)
  let id =
    InductiveDefinition.map IntroduceIfThenElse.apply ls
    |> InductiveDefinition.map (QuantifierElimination.apply SL_graph.empty)
  in
  let sid = InductiveDefinition.ID_map.of_list [("ls", id)] in
  let nx = SL.Term.mk_heap_term MemoryModel.Field.next x in
  let g = SL_graph.compute (SL_builtins.mk_pto_ls x ~next:y) in
  (* TODO: can we avoid simplification here? *)
  let actual = Simplifier.simplify @@ InductiveDefinition.unfold_guided sid id g [x; y] 10 in
  let expected =
    SL.mk_ite (SL.mk_eq [x; y]) SL.emp (SL_builtins.mk_pto_ls x ~next:nx)
  in
  SL.check_equal actual expected

let () =
  run "Inductive definitions" [
    "instantiate", [
      test_case "no refresh" `Quick instantiate_test1;
      test_case "refresh"    `Quick instantiate_test2;
    ];
    "unfold", [
      test_case "unfold ls, depth: 0"     `Quick unfold_test1;
      test_case "unfold ls, depth: 1"     `Quick unfold_test2;
      test_case "unfold ls, depth: 2"     `Quick unfold_test3;
      test_case "unfold ls-ite, depth: 0" `Quick unfold_test4;
      test_case "unfold ls-ite, depth: 1" `Quick unfold_test5;
    ];
    "unfold (branching)", [
      test_case "unfold tll, depth: 3"     `Quick unfold_tll_test1;
    ];
    "unfold (guided)", [
      test_case "unfold ls (x=y)" `Quick guided_unfold_test1;
      test_case "unfold ls (x |-> y)" `Quick guided_unfold_test2;
    ];
  ]

