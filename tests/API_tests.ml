(* Tests of Astral's API.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open SL_builtins
open SL_testable

let check_sat_test1 () =
  let solver = Solver.init () in
  let phi = SL.mk_star [SL_builtins.mk_ls x ~sink:y; SL_builtins.mk_ls x ~sink:nil] in
  assert (Solver.check_sat solver phi)

let check_sat_test2 () =
  let solver = Solver.init () in
  let phi = SL.mk_star [
    SL_builtins.mk_ls x ~sink:y;
    SL_builtins.mk_ls x ~sink:z;
    SL.mk_distinct [x; y; z]]
  in
  assert (not @@ Solver.check_sat solver phi)

let corner_case_test1 () =
  let solver = Solver.init () in
  let phi = SL_builtins.mk_ls nil ~sink:nil in
  assert (Solver.check_sat solver phi)


(** DLS *)

let root = SL.Term.mk_var "root" loc_dls
let sink = SL.Term.mk_var "sink" loc_dls
let root' = SL.Term.mk_var "root_" loc_dls
let sink' = SL.Term.mk_var "sink_" loc_dls

let dls_test1 () =
  let solver = Solver.init () in
  let phi = SL.mk_star [
    SL.mk_distinct [root; root'; sink; sink'];
    mk_dls root ~sink ~root' ~sink';
    mk_pto_dls root ~next:nil ~prev:nil;
    mk_pto_dls root' ~next:nil ~prev:nil;]
  in
  assert (not @@ Solver.check_sat solver phi)

let dls_test2 () =
  let solver = Solver.init () in
  let phi = mk_dls nil ~root':nil ~sink:nil ~sink':nil in
  assert (Solver.check_sat solver phi)

(** NLS *)

let root = SL.Term.mk_var "root" loc_nls
let sink = SL.Term.mk_var "sink" loc_nls
let bottom = SL.Term.mk_var "bottom" loc_nls

let nls_test1 () =
  let solver = Solver.init () in
  let phi = mk_nls root ~sink ~bottom in
  assert (Solver.check_sat solver phi)

let nls_test2 () =
  let solver = Solver.init () in
  let phi = mk_nls nil ~sink:nil ~bottom:nil in
  assert (Solver.check_sat solver phi)

(** Freed *)

let freed_test1 () =
  let solver = Solver.init () in
  let phi = SL_builtins.mk_freed x in
  assert (Solver.check_sat solver phi)

(** Custom memory model *)

let memory_model_test () =
  let open MemoryModel in
  let tree_sort = Sort.mk_loc "Tree" in
  let left = Field.mk "left" tree_sort in
  let right = Field.mk "right" tree_sort in
  let tree_struct = StructDef.mk "tree_t" ~cons:"tree_c" [left; right] in
  let x = SL.Term.mk_var "x" tree_sort in
  let phi = SL.mk_pto_struct x tree_struct [x; x] in

  let heap_sort = HeapSort.of_list [(tree_sort, tree_struct)] in
  let solver = Solver.init () |> Solver.set_heap_sort heap_sort in
  assert (Solver.check_sat solver phi)

let combined_memory_model_test () =
  let open MemoryModel in
  let tree_sort = Sort.mk_loc "Tree" in
  let left = Field.mk "left" tree_sort in
  let right = Field.mk "right" tree_sort in
  let tree_struct = StructDef.mk "tree_t" ~cons:"tree_c" [left; right] in
  let x = SL.Term.mk_var "x" tree_sort in
  let y = SL.Term.mk_var "y" SL_builtins.loc_dls in
  let phi = SL.mk_star [
    SL.mk_pto_struct x tree_struct [x; x];
    SL_builtins.mk_pto_dls y ~next:y ~prev:y]
  in

  let heap_sort = HeapSort.of_list [(tree_sort, tree_struct)] in
  let solver = Solver.init () |> Solver.set_heap_sort heap_sort in
  assert (Solver.check_sat solver phi)

(** Timeout *)

let timeout_template size init_to call_to expected_reason =
  let solver = match init_to with
    | None -> Solver.init ()
    | Some timeout -> Solver.init ~timeout ()
  in
  let phi = SL.mk_not @@ SL.mk_and [
    mk_ls x ~sink:y;
    SL.mk_not (SL.mk_star @@ List.init size (fun _ -> SL.mk_not SL.emp))
  ]
  in
  let res = match call_to with
    | None -> Solver.solve solver phi
    | Some timeout -> Solver.solve solver ~timeout phi
  in
  assert (match res with
    | `Unknown reason -> String.equal reason expected_reason
    | _ -> false
  )

let _timeout_backend_test1 () =
  timeout_template 40 (Some 1) None "canceled"

let _timeout_backend_test2 () =
  timeout_template 40 None (Some 1) "canceled"

let _timeout_astral_test1 () =
  timeout_template 100 (Some 1) None "astral timeout"

let _timeout_astral_test2 () =
  timeout_template 100 None (Some 1) "astral timeout"

(** Output *)

let debug_input () =
  let dirname = Format.asprintf "%s/_astral_test" (Filename.get_temp_dir_name ()) in
  let solver = Solver.init ~dump_queries:(`Full dirname) () in
  let phi = SL.emp in
  let _ = Solver.check_sat solver phi in
  assert (Sys.file_exists @@ dirname ^ "/query_0001/input.smt2")

let () =
  run "API" [
    "debug", [
      test_case "input"   `Quick debug_input;
    ];
    "Custom memory model", [
      test_case "tree ptr"             `Quick memory_model_test;
      test_case "tree ptr + dls ptr"   `Quick combined_memory_model_test;
    ];
    (*"timeout", [
      test_case "Backend timeout (init)"   `Quick timeout_backend_test1;
      test_case "Backend timeout (solve)"  `Quick timeout_backend_test2;
      test_case "Astral timeout (init)"    `Quick timeout_astral_test1;
      test_case "Astral timeout (solve)"   `Quick timeout_astral_test2;
    ];*)
    "check_sat", [
      test_case "sat"   `Quick check_sat_test1;
      test_case "unsat" `Quick check_sat_test2;
    ];
    "LS", [
      test_case "ls(nil, nil)" `Quick corner_case_test1;
    ];
    "DLS", [
      test_case "DLS - sat"    `Quick dls_test1;
      test_case "dls(nil, nil, nil, nil)" `Quick dls_test2;
    ];
    "NLS", [
      test_case "nls(x, y, z)"       `Quick nls_test1;
      test_case "nls(nil, nil, nil)" `Quick nls_test2;
    ];
    "freed", [
      test_case "freed(x)" `Quick freed_test1;
    ];


  ]
