(* Stack-heap models of separation logic.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

(** Representation of (sorted) memory locations. *)
module Location = struct

  type t = Int.t * Sort.t
  [@@deriving compare, equal]

  let mk n sort = (n, sort)

  let mk_ls n = (n, Sort.loc_ls)

  let mk_dls n = (n, Sort.loc_dls)

  let mk_nls n = (n, Sort.loc_nls)

  let mk_nil n = (n, Sort.loc_nil)

  let get_sort = snd

  let show (loc, _) = Format.asprintf "%d" loc

  let show_with_sort (loc, sort) = Format.asprintf "%d : %s" loc (Sort.show sort)

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Collections(Self)

end

module Footprint = struct
  include Set.Make(Location)

  let show fp =
    elements fp
    |> List.map Location.show
    |> String.concat ", "
    |> (fun set -> "{" ^ set ^ "}")

  module Self = struct
    type nonrec t = t
    let show = show
  end

  include Datatype.Printable(Self)

  end

module Value = struct

  type t =
    | LS of Location.t
    | DLS of Location.t * Location.t
    | NLS of Location.t * Location.t
  [@@deriving compare, equal]

  let mk_ls n = LS n

  let mk_dls ~next ~prev = DLS (next, prev)

  let mk_nls ~next ~top = NLS (next, top)

  let show = function
    | LS n -> Location.show n
    | DLS (n, p) -> Format.asprintf "<n: %s, p: %s>" (Location.show n) (Location.show p)
    | NLS (n, t) -> Format.asprintf "<n: %s, t: %s>" (Location.show n) (Location.show t)

  module Self = struct
    type nonrec t = t
    let show = show
    let compare = compare
  end

  include Datatype.Printable(Self)
  include Datatype.Comparable(Self)

end

module Stack = struct

  include SSL.Variable.MonoMap(Location)

  let to_smtlib stack =
    bindings stack
    |> List.map (fun (x, sx) -> Format.asprintf "(define-const %s Loc %s)"
        (SSL.Variable.show x) (Location.show sx))
    |> String.concat "\n"

end

module Heap = struct

  include Location.MonoMap(Value)

  let add_ls heap ~src ~next = add src (Value.mk_ls next) heap

  let add_dls heap ~src ~next ~prev = add src (Value.mk_dls ~next ~prev) heap

  let add_nls heap ~src ~next ~top = add src (Value.mk_nls ~next ~top) heap

  (** Partial field access *)

  let find_field field x heap = match field, find x heap with
    | SSL.Field.Next, Value.LS n -> n
    | SSL.Field.Next, Value.DLS (n, _) -> n
    | SSL.Field.Next, Value.NLS (n, _) -> n
    | SSL.Field.Prev, Value.DLS (_, p) -> p
    | SSL.Field.Top,  Value.NLS (_, t) -> t

  let show heap =
    bindings heap
    |> List.map (fun (loc, vals) ->
       Format.asprintf "%s -> %s" (Location.show loc) (Value.show vals))
    |> String.concat "\n"

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
  footprints : Footprint.t SSL.Map.t;   (* Mapping of sub-formulae to their footprints *)
  heaps : Heap.t SSL.Map.t;             (* Mapping of sub-formulae to their heaps *)
}

let empty = {
  stack = Stack.empty;
  heap = Heap.empty;

  footprints = SSL.Map.empty;
  heaps = SSL.Map.empty;
}

let init ?(footprints=SSL.Map.empty) ?(heaps=SSL.Map.empty) s h = {
  stack = s;
  heap = h;

  footprints = footprints;
  heaps = heaps;
}

let get_stack sh = sh.stack

let get_heap sh = sh.heap

let get_domain sh = Footprint.of_list @@ List.map fst @@ Heap.bindings sh.heap

let get_footprint sh psi = SSL.Map.find psi sh.footprints

(** Relevant only for septraction sub-formulae *)
let get_subformula_model sh psi = {sh with heap = SSL.Map.find psi sh.heaps}

let stack_inverse sh loc =
  Stack.fold
    (fun x loc' acc ->
      if Location.equal loc loc'
      then SSL.Variable.show x :: acc else acc
  ) sh.stack []

(** {2 Pretty printers} *)

let show sh =
  let str =  "Stack:\n" in
  let str = Stack.fold
    (fun k v acc -> Format.asprintf "%s\t%s -> %s\n" acc (SSL.Variable.show k) (Location.show v))
    sh.stack
    str
  in
  let str = str ^ "\nHeap:\n" in
  let str =  str ^ Heap.show sh.heap in
  let str = str ^ "\nFootprints:\n" in
  SSL.Map.fold
    (fun psi fp acc ->
      Format.asprintf "%s\t%a -> {%s}\n" acc SSL.pp psi (Footprint.show fp)
    ) sh.footprints str
  |> SSL.Map.fold
    (fun psi heap acc ->
      Format.asprintf "%s\nWitness heap of %a:\n%s" acc SSL.pp psi (Heap.show heap)
    ) sh.heaps

let print sh =
  Format.printf "Stack:\n";
  Stack.iter (fun k v -> Format.printf "%s -> %s\n" (SSL.Variable.show k) (Location.show v)) sh.stack;
  Format.printf "Heap (next):\n";
  Heap.iter (fun k vals -> Format.printf "%s -> %s\n" (Location.show k) (Value.show vals)) sh.heap;
  Format.printf "Footprints:\n";
  SSL.Map.iter
    (fun psi fp -> Format.printf "%a -> {%s}\n" SSL.pp psi (Footprint.show fp)) sh.footprints

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

module Vertex = struct
  include Location
  let hash = Hashtbl.hash
end

module EdgeLabel = struct
  include SSL.Field
  let default = SSL.Field.Next
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

  let has_path g ~src ~dst =
    let path_checker = PathChecker.create g in
    PathChecker.check_path path_checker src dst

  let get_path g ~src ~dst =
    try
      let path, _ = Dijkstra.shortest_path g src dst in
      List.map E.src path

    (* If there is no path, return empty *)
    with Not_found -> []

  let model = ref None

  module Dot = Graphviz.Dot
    (struct
      include Graph

      let graph_attributes _ = []
      let default_vertex_attributes v = []
      let vertex_name v = Location.show v
      let vertex_attributes v =
        [`Label (Format.asprintf "%s : %s" (Location.show v) (String.concat "," (stack_inverse (Option.get !model) v)))]

      let get_subgraph _ = None
      let edge_attributes e = [
        `Style (match E.label e with Next -> `Solid | Prev -> `Dashed | Top -> `Bold);
      ]
      let default_edge_attributes _ = []
    end)

end

let update x y g = match y with
  | Value.LS n-> HeapGraph.add_edge_e g (x, Next, n)
  | Value.DLS (n, p) ->
    let g = HeapGraph.add_edge_e g (x, Next, n) in
    HeapGraph.add_edge_e g (x, Prev, p)
  | Value.NLS (n, t) ->
    let g = HeapGraph.add_edge_e g (x, Next, n) in
    HeapGraph.add_edge_e g (x, Top, t)

let get_heap_graph sh =
  HeapGraph.empty
  |> Stack.fold (fun _ loc g -> HeapGraph.add_vertex g loc) sh.stack
  |> Heap.fold update sh.heap

let domain sh = Footprint.of_list @@ List.map fst @@ Heap.bindings sh.heap

let has_path sh ~src ~dst = HeapGraph.has_path (get_heap_graph sh) ~src ~dst

let get_path sh ~src ~dst = HeapGraph.get_path (get_heap_graph sh) ~src ~dst

let output_graph filename sh =
  let channel = open_out filename in
  HeapGraph.model := Some sh;
  HeapGraph.Dot.output_graph channel (get_heap_graph sh);
  close_out channel
