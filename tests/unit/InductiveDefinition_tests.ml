(* Tests for operations over inductive definitions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open InductiveDefinition_testable

module SL = SL_testable
open SL

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
let sid_ls = SID.register_user_defined SID.empty ls

let unfold_test1 () =
  let bound = UnfoldingBound.empty in
  let actual = SID.unfold sid_ls "ls" [x; y] bound in
  let expected = SL.mk_eq [x; y] in
  SL.check_equal actual expected

let unfold_test2 () =
  let bound = UnfoldingBound.singleton SL_builtins.loc_ls 1 in
  let actual = SID.unfold sid_ls "ls" [x; y] bound in
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
  let bound = UnfoldingBound.singleton SL_builtins.loc_ls 2 in
  let actual = SID.unfold sid_ls "ls" [x; y] bound in
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
  (* TODO: workaround *)
  GlobalSID.reset ();
  GlobalSID.register_user_defined ls;

  let id = InductiveDefinition.map IntroduceIfThenElse.apply ls in
  let sid = SID.register_user_defined SID.empty id in
  let bound = UnfoldingBound.empty in
  let actual = SID.unfold sid "ls" [x; y] bound in
  let expected = SL.mk_eq [x; y] in
  SL.check_equal actual expected

let unfold_test5 () =
  (* TODO: workaround *)
  GlobalSID.reset ();
  GlobalSID.register_user_defined ls;

  let id = InductiveDefinition.map IntroduceIfThenElse.apply ls in
  let sid = SID.register_user_defined SID.empty id in
  let bound = UnfoldingBound.singleton SL_builtins.loc_ls 1 in
  let actual = SID.unfold sid "ls" [x; y] bound in
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
  let sid = SID.register_user_defined SID.empty TLL.id in
  let bound = UnfoldingBound.singleton sort 2 in
  let [x; y; z] = List.map SL.Term.of_var @@ SL.Variable.mk_list sort ["x"; "y"; "z"] in
  let actual = SID.unfold sid "tll" [x; y; z] bound in
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
  ]

