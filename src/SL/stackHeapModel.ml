(* Stack-heap models of separation logic.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)


open MemoryModel

(** Representation of (sorted) memory locations. *)
module Location = struct

  type kind =
    | Implicit of Int.t  (** Used for uninterpreted locations *)
    | SMT of Constant.t  (** Used for interpreted locations *)
  [@@deriving compare, equal]

  type t = kind * Sort.t
  [@@deriving compare, equal]

  let mk n sort = (Implicit n, sort)

  let mk_smt c = (SMT c, Constant.get_sort c)

  let mk_ls n = (Implicit n, Sort.loc_ls)

  let mk_nil n = (Implicit n, Sort.loc_nil)

  let get_sort = snd

  let has_sort sort loc = Sort.equal sort (get_sort loc)

  let show (loc, _) = match loc with
    | Implicit i -> string_of_int i
    | SMT const -> Constant.show const

  let show_with_sort ((loc, sort) as l) = Format.asprintf "%s : %s" (show l)  (Sort.show sort)

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Collections(Self)

end

module Footprint = struct

  include Location.Set

  let rec disjoint_list = function
    | [] | [_] -> true
    | xs1 :: xs2 :: rest -> disjoint xs1 xs2 && disjoint_list (union xs1 xs2 :: rest)

end

module Value = struct

  type t =
    | Struct of StructDef.t * Location.t List.t
    | Data of Constant.t
  [@@deriving compare, equal]

  let mk_struct def locs = Struct (def, locs)

  let mk_data c = Data c

  let show = function
    | Struct (def, locs) -> Format.asprintf "%s(%s)" (StructDef.show_cons def)
                              (String.concat " " @@ List.map Location.show locs)
    | Data c -> Constant.show c

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Comparable(Self)

end

module Stack = struct

  module M = SL.Term.MonoMap(Location)

  type t = {
    model : SMT.Model.t;
    stack : M.t;
  }

  let empty = {model = SMT.Model.empty; stack = M.empty}

  let mk model stack = {model; stack}

  let eval stack term =
    try M.find term stack.stack
    with Not_found -> match SL.Term.view term with SmtTerm t -> Location.mk_smt @@ SMT.Model.eval stack.model t

  let add x y s = {s with stack = M.add x y s.stack}

  let base self =
    M.filter (fun t _ -> match SL.Term.view t with Var _ -> true | _ -> false) self.stack

  let to_smtlib stack =
    base stack
    |> M.bindings
    |> List.map (fun (x, sx) -> Format.asprintf "(define-const %s Loc %s)"
        (SL.Term.show x) (Location.show sx))
    |> String.concat "\n"

end

module Heap = struct

  include Location.MonoMap(Value)

  (** Partial field access *)

  let find_field field x heap = match find x heap with
    | Struct (def, xs) ->
      let index = StructDef.field_index def field in
      List.nth xs index
    | Data _ -> failwith "[TODO] Heap.find_field: data"

  let to_smtlib heap =
    bindings heap
    |> List.map (fun (loc, vals) ->
       Format.asprintf "(define-const h(%s) Loc %s)" (Location.show loc) (Value.show vals))
    |> String.concat "\n"

  module Self = struct
    type nonrec t = t
    let show = show
  end

  include Datatype.Printable(Self)

end

type t = {
  stack : Stack.t;
  heap : Heap.t;

  (* Additional certificates *)
  footprints : Footprint.t SL.Map.t;   (* Mapping of sub-formulae to their footprints *)
  heaps : Heap.t SL.Map.t;             (* Mapping of sub-formulae to their heaps *)
}

let empty = {
  stack = Stack.empty;
  heap = Heap.empty;

  footprints = SL.Map.empty;
  heaps = SL.Map.empty;
}

let init ?(footprints=SL.Map.empty) ?(heaps=SL.Map.empty) s h = {
  stack = s;
  heap = h;

  footprints = footprints;
  heaps = heaps;
}

let eval sh l = Stack.eval sh.stack l

let get_stack sh = sh.stack

let get_heap sh = sh.heap

let get_domain sh = Footprint.of_list @@ List.map fst @@ Heap.bindings sh.heap

let get_footprint sh psi = SL.Map.find psi sh.footprints

(** Relevant only for septraction sub-formulae *)
let get_subformula_model sh psi = {sh with heap = SL.Map.find psi sh.heaps}

let stack_inverse sh loc =
  Stack.M.fold
    (fun x loc' acc ->
      if Location.equal loc loc'
      then SL.Term.show x :: acc else acc
  ) sh.stack.stack []

(** {2 Pretty printers} *)

let show sh =
  let str =  "Stack:\n" in
  let str = Stack.M.fold
    (fun k v acc -> Format.asprintf "%s\t%s -> %s\n" acc (SL.Term.show k) (Location.show v))
    sh.stack.stack
    str
  in
  let str = str ^ "\nHeap:\n" in
  let str =  str ^ Heap.show sh.heap in
  let str = str ^ "\nFootprints:\n" in
  SL.Map.fold
    (fun psi fp acc ->
      Format.asprintf "%s\t%a -> {%s}\n" acc SL.pp psi (Footprint.show fp)
    ) sh.footprints str
  |> SL.Map.fold
    (fun psi heap acc ->
      Format.asprintf "%s\nWitness heap of %a:\n%s" acc SL.pp psi (Heap.show heap)
    ) sh.heaps

let print sh =
  Format.printf "Stack:\n";
  Stack.M.iter (fun k v -> Format.printf "%s -> %s\n" (SL.Term.show k) (Location.show v)) sh.stack.stack;
  Format.printf "Heap (next):\n";
  Heap.iter (fun k vals -> Format.printf "%s -> %s\n" (Location.show k) (Value.show vals)) sh.heap;
  Format.printf "Footprints:\n";
  SL.Map.iter
    (fun psi fp -> Format.printf "%a -> {%s}\n" SL.pp psi (Footprint.show fp)) sh.footprints

module Self = struct
  type nonrec t = t
  let show = show
end

include Datatype.Printable(Self)

(** TODO: sort declaration *)
let to_smtlib sh =
  Format.asprintf "(declare-sort Loc 0)\n%s\n%s"
    (Stack.to_smtlib sh.stack)
    (Heap.to_smtlib sh.heap)

(* === Graph representation of (s,h) model === *)

open Graph

(** This is a hack to access stack from vertex labelling function in Dot *)
let model = ref None

module Vertex = struct
  type t =
    | Loc of Location.t
    | Nil of Int.t
  [@@deriving equal, compare]

  let name = function
    | Loc loc -> Format.asprintf "\"%s\"" (Location.show loc)
    | Nil n -> Format.asprintf "\"nil_%d\"" n

  let show = function
    | Loc loc ->
      Format.asprintf "%s : %s"
        (Location.show loc)
        (String.concat "," (stack_inverse (Option.get !model) loc))
    | Nil n -> !UnicodeSymbols.bottom

  let hash = Hashtbl.hash
end

module EdgeLabel = struct
  include MemoryModel.Field
  let default = MemoryModel.Field.next
end

module HeapGraph = struct

  module Graph = Persistent.Digraph.ConcreteBidirectionalLabeled(Vertex)(EdgeLabel)
  include Graph

  module Labels = Map.Make(Vertex)

  module PathChecker = Path.Check(Graph)
  module Dijkstra = Path.Dijkstra(Graph)
                      (struct
                        type edge = Graph.E.t
                        include Int
                        let weight _ = 1
                        let add x y = x + y
                        let zero = 0
                      end)

  let filter_field g field =
    Graph.fold_edges_e (fun ((src, field', dst) as e) acc ->
      if Field.equal field field' then Graph.add_edge_e acc e
      else acc
    ) g Graph.empty

  let has_path g field ~src ~dst =
    let g = match field with
      | None -> g
      | Some field -> filter_field g field
    in
    let path_checker = PathChecker.create g in
    PathChecker.check_path path_checker src dst

  module Dot = Graphviz.Dot
    (struct
      include Graph

      let graph_attributes _ = []
      let default_vertex_attributes v = []
      let vertex_name = Vertex.name
      let vertex_attributes v = [
        `Label (Vertex.show v);
        `Shape (match v with Loc _ -> `Box | Nil _ -> `Plaintext);
      ]
      let get_subgraph _ = None
      let edge_attributes e = [
        `Label (Field.show @@ E.label e);
      ]
      let default_edge_attributes _ = []
    end)

  let get_path g field ~src ~dst =
    let g = match field with
      | None -> g
      | Some f -> filter_field g f
    in
    if not @@ mem_vertex g src then []
    else try
      let path, _ = Dijkstra.shortest_path g src dst in
      List.map E.src path

    (* If there is no path, return empty *)
    with Not_found -> []

  let get_nested_path g ~src ~dst ~sink ~field1 ~field2 =
    let top_path = get_path g ~src ~dst (Some field1) in
    List.map (fun loc -> get_path g ~src:loc ~dst:sink (Some field2)) top_path

  let has_nested_path g ~src ~dst ~sink ~field1 ~field2 =
    let top_path = get_path g ~src ~dst (Some field1) in
    has_path g (Some field1) ~src ~dst
    && List.for_all (fun loc -> has_path g (Some field2) ~src:loc ~dst:sink) top_path

  let get sh =
    let update x y g = match y with
      | Value.Struct (def, ys) ->
      let fields = StructDef.get_fields def in
      List.fold_left2 (fun acc field y -> add_edge_e acc (Loc x, field, Loc y)) g fields ys
    in
    empty
    |> Stack.M.fold (fun _ loc g -> add_vertex g (Loc loc)) sh.stack.stack
    |> Heap.fold update sh.heap

  (** Replace each link to nil with a fresh node
      TODO: add nils directly
  *)
  let prettify_nils sh g =
    let is_nil loc = List.mem "nil" @@ stack_inverse sh loc in
    let fresh_nil =
      let cnt = ref 0 in (* TODO *)
      (fun () -> cnt := !cnt - 1; Vertex.Nil !cnt)
    in
    let isolated_vertices =
      fold_vertex (fun v acc ->
        if out_degree g v = 0 then add_vertex acc v
        else acc
      ) g empty
    in
    fold_edges_e (fun (src, label, Loc dst) acc ->
      if is_nil dst then add_edge_e acc (src, label, fresh_nil ())
      else add_edge_e acc (src, label, Loc dst)
    ) g isolated_vertices

  let output channel sh =
    let g = get sh in
    let g = prettify_nils sh g in
    Dot.output_graph channel g

end

let succ_field sh field loc =
  Heap.find_field field loc sh.heap

let domain sh = Footprint.of_list @@ List.map fst @@ Heap.bindings sh.heap

let has_path ?field sh ~src ~dst =
  HeapGraph.has_path (HeapGraph.get sh) field ~src:(Loc src) ~dst:(Loc dst)

let get_path ?field sh ~src ~dst =
  HeapGraph.get_path (HeapGraph.get sh) field ~src:(Loc src) ~dst:(Loc dst)
  |> List.map (fun (Vertex.Loc l) -> l)

let get_nested_path sh ~src ~dst ~sink ~field1 ~field2 =
  HeapGraph.get_nested_path (HeapGraph.get sh) ~src:(Loc src) ~dst:(Loc dst) ~sink:(Loc dst) ~field1 ~field2
  |> List.map (fun xs -> List.map (fun (Vertex.Loc l) -> l) xs)

let has_nested_path sh ~src ~dst ~sink ~field1 ~field2 =
  HeapGraph.has_nested_path (HeapGraph.get sh) ~src:(Loc src) ~dst:(Loc dst) ~sink:(Loc dst) ~field1 ~field2

let output_graph filename sh =
  let channel = open_out filename in
  model := Some sh;
  HeapGraph.output channel sh;
  close_out channel
