(* Rewriters for inline tests with Alcotest.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2026 *)

open Ppxlib

let test_name_of_pattern ~loc pat =
  match pat.ppat_desc with
  | Ppat_var { txt; _ } -> txt                     (* let%test name = ... *)
  | Ppat_constant (Pconst_string (s, _, _)) -> s   (* let%test "long name" = ... *)
  | Ppat_any -> "<anonymous>"                      (* let%test _ = ... *)
  | _ -> Location.raise_errorf ~loc "let%%test: expected a name, a string or _"

let extension_suite =
  Extension.V3.declare "suite" Extension.Context.structure_item
    Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
    (fun ~ctxt suite_name ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      let open Ast_builder.Default in
      [%stri let () = Registry.set_suite [%e estring ~loc suite_name]])

let expand ~ctxt (vb : value_binding) =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let name = test_name_of_pattern ~loc vb.pvb_pat in
  let open Ast_builder.Default in
  [%stri
    let () =
      Registry.register_test
        ~name:[%e estring ~loc name]
        (fun () -> [%e vb.pvb_expr])
  ]

let extension_test =
  Extension.V3.declare
    "test"
    Extension.Context.structure_item
    Ast_pattern.(pstr (pstr_value nonrecursive (__ ^:: nil) ^:: nil))
    expand

let () =
  Ppxlib.Driver.register_transformation "ppx_alcotest"
    ~rules: [
      Ppxlib.Context_free.Rule.extension extension_suite;
      Ppxlib.Context_free.Rule.extension extension_test;
    ]
