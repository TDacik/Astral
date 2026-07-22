(* Smart constructors and typechecking functions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open BaseLogic_terms
open BaseLogic_config

(** Type checking *)

type type_error = string * string * Sort.t * t

let show_type_error (what, expects, sort, term) =
  Format.asprintf "%s expects %s, but got term of sort %s:\n %s"
    what expects (Sort.show sort) (show term)

exception TypeError of type_error

let () =
  Printexc.register_printer (function
    | TypeError ((what, expects, sort, term) as e) ->
      Some (show_type_error e)
    | _ -> None
  )

let check_type_prop ~what ~expects pred term =
  let sort = get_sort term in
  if pred sort then ()
  else raise (TypeError (what, expects, sort, term))

let check_type ~what sort term =
  let expects = "sort " ^ Sort.show sort in
  check_type_prop ~what ~expects (Sort.equal_mod_nil sort) term

let check_types ~what sorts terms =
  if List.compare_lengths sorts terms <> 0
  then
    failwith "TODO: incorrect number of params"
  else
    BatList.iter2i (fun i -> check_type ~what:(Format.asprintf "%s (param #%d)" what (i+1))) sorts terms

let check_same_type ~what = function
  | [] -> ()
  | x :: xs ->
    let sort = get_sort x in
    List.iteri (fun i t -> check_type ~what:(Format.asprintf "%s (param #%d)" what (i+2)) sort t) xs


(** Smart constructors *)

let mk_smart_app_aux app neutral anihilator operands =
  let is_neutral x = match neutral with Some n when equal x n -> true | _ -> false in
  let is_anihilator x = match anihilator with Some a when equal x a -> true | _ -> false in
  let operands =
    if List.exists is_anihilator operands then [Option.get anihilator]
    else List.filter (fun x -> not @@ is_neutral x) operands
  in
  let neutral = Option.value neutral ~default:(Application (app, [])) in
  let operands' = List.fold_left (fun acc -> function
    | Application (app', xs') when Application.equal app app' -> acc @ xs'
    | x -> acc @ [x]
  ) [] operands in
  match operands' with
    | [] -> neutral
    | [x] -> x
    | xs -> Application (app, xs)

let mk_smart_app app ?neutral ?anihilator
  (*?(commutativy=false)
    ?(associative=false)*)
  operands =
    (* We can still do some basic simplification *)
    if not @@ !do_simplification then match operands with
      | [x] -> x
      | _ -> Application (app, operands)
    else mk_smart_app_aux app neutral anihilator operands
