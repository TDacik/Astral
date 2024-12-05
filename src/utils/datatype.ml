(* Smart datatypes
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Datatype_sig

module Printable (M : SHOW) = struct

  include M

  let pp fmt x = Format.fprintf fmt "%s" (M.show x)

  let print ?(prefix="") x =
    let prefix = if prefix = "" then prefix else prefix ^ " " in
    Format.printf "%s%s\n" prefix (M.show x)

  let dump filename x =
    let channel = open_out_gen [Open_creat; Open_wronly] 0o666 filename in
    Printf.fprintf channel "%s\n" (M.show x);
    close_out channel

  let show_list ?(separator=", ") xs = String.concat separator @@ List.map M.show xs

  let pp_list (*(separator=", ")*) fmt xs = Format.fprintf fmt "%s" (show_list ~separator:"," xs)

  let print_list ?(separator=", ") ?(prefix="") xs = Format.printf "%s%s\n" prefix (show_list ~separator xs)

end

module Comparable (M : COMPARISON) = struct

  include M

  let equal lhs rhs = compare lhs rhs == 0

end

let show_map_aux show_key show_val = function
  | [] -> "{}"
  | bindings ->
    bindings
    |> List.map (fun (k, v) -> Format.asprintf "    %s : %s" (show_key k) (show_val v))
    |> String.concat "\n"
    |> (fun str -> "{\n" ^ str ^ "\n  }")

module Collections (M : COMPARISON) = struct

  type t = M.t [@@ ocaml.warning "-34"]

  module Set = struct

    include BatSet.Make(M)

    let show set =
      if is_empty set then "{}"
      else
        elements set
        |> List.map M.show
        |> String.concat ","
        |> (fun str -> "{" ^ str ^ "}\n")

     include Printable(struct
       type nonrec t = t
       let show = show
     end)


  end

  module Map = struct

    include BatMap.Make(M)

    let keys map = List.map fst @@ bindings map

    let values map = List.map snd @@ bindings map

    let of_list xs = List.to_seq xs |> of_seq

    let find_pred pred map =
      filter (fun k _ -> pred k) map
      |> any
      |> fst

    let show show_data map = show_map_aux M.show show_data (bindings map)

    let show_custom show_key show_data map = show_map_aux show_key show_data (bindings map)
      (*if is_empty map then "{}"
      else
        bindings map
        |> List.map (fun (k, v) -> Format.asprintf "    %s : %s" (M.show k) (show_data v))
        |> String.concat "\n"
        |> (fun str -> "{\n" ^ str ^ "\n  }\n")
  *)
  end

  module MonoMap(Data : SHOW) = struct

    type data = Data.t

    (* Include polymorphic map and fix its type to Data.t *)
    include BatMap.Make(M)
    type nonrec t = Data.t t

    let keys map = List.map fst @@ bindings map
    let values map = List.map snd @@ bindings map

    let of_list xs = List.to_seq xs |> of_seq

    let find_pred pred map =
      filter (fun k _ -> pred k) map
      |> any
      |> fst

    let show map = show_map_aux M.show Data.show (bindings map)

    let show_custom show_key show_data map = show_map_aux show_key show_data (bindings map)

  end

end
