
(** Test values *)

let x = SL.Term.mk_var "x" SL_builtins.loc_ls
let y = SL.Term.mk_var "y" SL_builtins.loc_dls
let z = SL.Term.mk_var "z" SL_builtins.loc_dls

let is_type_err = function
  | BaseLogic.TypeError _ -> true
  | _ -> false

(** Tests *)

let pto_ok () =
  ignore @@ SL.mk_pto_struct x DLS.struct_dls [y; y]

let pto_err () =
  let f () = ignore @@ SL.mk_pto_struct x DLS.struct_dls [x; y] in
  match_raises "type error" is_type_err f


let eq_ok () =
  ignore @@ SL.mk_eq [y; z]

let eq_err () =
  let f () = ignore @@ SL.mk_eq [x; y] in
  match_raises "type_error" is_type_err f


let distinct_ok () =
  ignore @@ SL.mk_distinct [y; z]

let distinct_err () =
  let f () = ignore  @@ SL.mk_distinct [x; y] in
  match_raises "type error" is_type_err f

let () =
  run "SL API" [
    "Dynamic type checks", [
      test_case "pto_ok"        `Quick pto_ok;
      test_case "pto_err"       `Quick pto_err;
      test_case "eq_ok"         `Quick eq_ok;
      test_case "eq_err"        `Quick eq_err;
      test_case "distinct_ok"   `Quick distinct_ok;
      test_case "distinct_err"  `Quick distinct_err;
    ];
  ]
