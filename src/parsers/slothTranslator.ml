(* Translation to input format of Sloth
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL

module F = Format

let translate_var var = match var with
  | Variable.Var _ -> Variable.show var
  | Variable.Nil -> "sl.list.null"

let translate_vars phi =
  SSL.get_vars phi
  |> List.filter (fun v -> not @@ SSL.Variable.is_nil v)
  |> List.map translate_var
  |> List.map (F.asprintf "(declare-const %s sl.list.loc)")
  |> String.concat "\n"

let rec translate = function
  | And (f1, f2) -> F.asprintf "(and %s %s)\n" (translate f1) (translate f2)
  | Or (f1, f2) -> F.asprintf "(or %s %s)\n" (translate f1) (translate f2)
  | Not f ->  F.asprintf "(not %s)\n" (translate f)
  | GuardedNeg (f1, f2) ->  F.asprintf "(and %s (not %s))\n" (translate f1) (translate f2)
  | Star (f1, f2) ->  F.asprintf "(sl.sepcon %s %s)\n" (translate f1) (translate f2)
  | LS (v1, v2) -> F.asprintf "(sl.list.seg %s %s)\n" (translate_var v1) (translate_var v2)
  | PointsTo (v1, v2) -> F.asprintf "(sl.list.next %s %s)\n" (translate_var v1) (translate_var v2)
  | Eq (v1, v2) -> F.asprintf "(sl.list.eq %s %s)\n" (translate_var v1) (translate_var v2)
  | Neq (v1, v2) -> F.asprintf "(sl.list.neq %s %s)\n" (translate_var v1) (translate_var v2)

  | Septraction (f1, f2) -> failwith "Not supported"

let translate_all phi = (translate_vars phi) ^ "\n\n(assert " ^ (translate phi) ^ ")\n"

let dump file phi =
  let channel = open_out_gen [Open_creat; Open_wronly] 0o666 file in
  Printf.fprintf channel "%s\n" (translate_all phi);
  close_out channel
