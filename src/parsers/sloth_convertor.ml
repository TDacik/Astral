(* Translation to input format of Sloth
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL

module F = Format

let name = "sloth"

let convert_var var = match var with
  | Variable.Var _ -> Variable.show var
  | Variable.Nil -> "sl.list.null"

let convert_vars phi =
  SSL.get_vars phi
  |> List.filter (fun v -> not @@ SSL.Variable.is_nil v)
  |> List.map convert_var
  |> List.map (F.asprintf "(declare-const %s sl.list.loc)")
  |> String.concat "\n"

let rec convert = function
  | And (f1, f2) -> F.asprintf "(and %s %s)\n" (convert f1) (convert f2)
  | Or (f1, f2) -> F.asprintf "(or %s %s)\n" (convert f1) (convert f2)
  | Not f ->  F.asprintf "(not %s)\n" (convert f)
  | GuardedNeg (f1, f2) ->  F.asprintf "(and %s (not %s))\n" (convert f1) (convert f2)
  | Star (f1, f2) ->  F.asprintf "(sl.sepcon %s %s)\n" (convert f1) (convert f2)
  | LS (v1, v2) -> F.asprintf "(sl.list.seg %s %s)\n" (convert_var v1) (convert_var v2)
  | PointsTo (v1, v2) -> F.asprintf "(sl.list.next %s %s)\n" (convert_var v1) (convert_var v2)
  | Eq (v1, v2) -> F.asprintf "(sl.list.eq %s %s)\n" (convert_var v1) (convert_var v2)
  | Neq (v1, v2) -> F.asprintf "(sl.list.neq %s %s)\n" (convert_var v1) (convert_var v2)

  | Septraction (f1, f2) -> failwith "Not supported"

let convert_all phi = (convert_vars phi) ^ "\n\n(assert " ^ (convert phi) ^ ")\n"

let dump file phi =
  let channel = open_out_gen [Open_creat; Open_wronly] 0o666 file in
  Printf.fprintf channel "%s\n" (convert_all phi);
  close_out channel
