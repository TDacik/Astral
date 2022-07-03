(* Bounded unfolding of recursive predicates
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL
open Batteries

let name = "predicate unfolding"

let generate_locs n =
  List.range 1 `To n
  |> List.map (fun i -> Format.asprintf "loc%d" i)
  |> List.map Variable.mk

let rec unfold_ls locs n x y = match n with
  | 0 -> Eq (x,y)
  | n ->
    let aux = List.map
      (fun l ->
        let ptr = PointsTo (x, l) in
        let unfolding = unfold_ls (List.remove locs l) (n-1) l y in
        Star (ptr, unfolding)
      ) locs
    in
    let ls = Star (Neq (x,y), mk_or aux) in
    Or (Eq (x,y), ls)

let unfold_ls n x y =
  let locs = generate_locs n in
  unfold_ls locs n x y

let rec unfold phi n = match phi with
  | And (f1, f2) -> And (unfold f1 n, unfold f2 n)
  | Or (f1, f2) -> Or (unfold f1 n, unfold f2 n)
  | Not f -> Not (unfold f n)
  | GuardedNeg (f1, f2) -> GuardedNeg (unfold f1 n, unfold f2 n)
  | Star (f1, f2) -> Star (unfold f1 n, unfold f2 n)
  | Septraction (f1, f2) -> Septraction (unfold f1 n, unfold f2 n)
  | LS (x, y) -> unfold_ls n x y
  | atom -> atom

(* TODO: bound *)
let convert phi = SSL.show @@ unfold phi 10

let dump file phi =
  let channel = open_out_gen [Open_creat; Open_wronly] 0o666 file in
  Printf.fprintf channel "%s\n" (convert phi);
  close_out channel
