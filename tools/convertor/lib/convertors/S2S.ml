(* Conversion to the input format of the S2S solver:
 *  - https://loc.bitbucket.io/s2s/
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

open MemoryModel

open Convertor_sig
open Utils

module Self = struct

  let params = {
    name = "s2s";
    suffix = ".ss";
    supports_sat = true;
    supports_variadic_ops = false;
    precise_semantics = false;
  }

  let ctx = ref Context.empty

  let init context = ctx := context

  let convert_var var =
    let name = SL.Variable.show var in
    BatString.nreplace ~str:name ~sub:"!" ~by:"_"

  let convert phi =
    let open SL_printer in
    let p = {
      eq = "=";
      neq = "!=";
      pto = "::";
      star = "*";
      and_ = "&";
      emp = "emp";

      true_ = "true";
      false_ = "false";

      existential = "exists";
      qf_separator = ", ";
      qf_dot = ":";

      struct_begin = "<";
      struct_end = ">";
      struct_separator = ", ";

      nil = "null";

      print_var = convert_var;
      print_struct = StructDef.get_name;
    }
    in
    SL_printer.print_symbolic_heap ~precise:false ~p phi

  let comment comment = "// " ^ comment

  (** Declarations *)

  (* S2S does not declare variables and sorts *)
  let declare_sort sort = comment @@ ("sort " ^ Sort.show sort)
  let declare_var var = comment @@ ("variable " ^ SL.Variable.show_with_sort var)
  let declare_heap_sort hs = comment @@ ("heap sort " ^ HeapSort.show hs)

  (** S2S does not distinguish between structure and pointer to structure. *)
  let convert_sort sort =
    StructDef.get_name @@ HeapSort.find_target sort !ctx.heap_sort

  let declare_struct (def : StructDef.t) =
    let fields =
      List.map (fun f -> Format.asprintf "  %s %s;\n" (convert_sort @@ Field.get_sort f) (Field.show f)) def.fields
      |> String.concat ""
    in
    Format.asprintf "ddata %s {\n%s}."
      (StructDef.get_name def)
      fields

  let declare_predicate (def : InductiveDefinition.t) =
    let header =
      List.map (fun v -> Format.asprintf "%s:%s" (convert_var v) (convert_sort @@ SL.Variable.get_sort v)) def.header
      |> String.concat ","
    in
    let cases =
      List.map convert (InductiveDefinition.cases def)
      |> String.concat "\n  or "
    in
    Format.asprintf "pred %s< %s > ==\n  %s."
      def.name
      header
      cases

  let set_status status = comment ("expected status: " ^ status_to_string status)

  let add_check_sat phi = match PreciseToImprecise.as_imprecise_query phi with
    | SymbolicHeap_SAT psi -> ("checksat " ^ convert psi ^ ".")
    | SymbolicHeap_ENTL (lhs, rhs) -> ("checkent " ^ convert lhs ^ " |- " ^ convert rhs ^ ".")
    | _ -> raise @@ NotSupported ("not sat/entailment" ^ SL.show phi)
end

include ConvertorBuilder.Make(Self)
