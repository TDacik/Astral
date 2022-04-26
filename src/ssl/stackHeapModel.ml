(* Representation of stack-heap models.
 *
 * Stack-heap model is represented by a pair of maps:
 *   - s : Vars -> Int,
 *   - h : Int  -> Int.
 *
 * Alternatively, the model can be accessed as an
 * oriented graph where vertices are heap locations
 * labeled by set of names.
 *
 * A model also contains additional certificates:
 *  - footprints of positive subformulas
 *  - auxiliary heaps
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2021 *)

open Graph

module Map = BatMap
module List = BatList

module Location = Int

module Footprint = struct
  include Set.Make(Int)
  let show fp =
    let names = fold (fun x acc -> Format.asprintf "%d" x :: acc) fp [] in
    "{" ^ String.concat ", " names ^ "}"
end

module Stack = struct
  include Map.Make(SSL.Variable)

  let find var stack =
    try find var stack
    with Not_found ->
      failwith (Format.asprintf
        "Internal error: Stack image of variable %a is undefined" SSL.Variable.pp var
      )

end

module Heap = Map.Make(Location)

type t = {
  stack : Location.t Stack.t;
  heap : Location.t Heap.t;

  (* Additional certificates *)
  footprints : Footprint.t SSL.Map.t;   (* Mapping of subformulas to their footprints *)
  heaps : Location.t Heap.t SSL.Map.t;  (* Mapping of subformulas to their heaps *)
}

let empty () = {
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

let get_footprint sh psi =
  try SSL.Map.find psi sh.footprints
  with Not_found ->
    failwith (Format.asprintf
      "Internal error: No footprint certificate for sub-formula %a" SSL.pp psi
    )

(** Relevant only for subformulas of septraction *)
let get_subformula_model sh psi =
  try {sh with heap = SSL.Map.find psi sh.heaps}
  with Not_found ->
    failwith (Format.asprintf
      "Internal error: No witness heap for sub-formula %a" SSL.pp psi
    )

let stack_inverse sh loc =
  Stack.fold
    (fun x loc' acc ->
      if Int.equal loc loc'
      then SSL.Variable.show x :: acc else acc
  ) sh.stack []

(** {2 Pretty printers} *)

let fp_to_string fp = String.concat ", " @@ List.map Int.to_string @@ Footprint.elements fp

let heap_to_string heap =
  Heap.fold (fun k v acc -> Format.asprintf "%s\t%d -> %d\n" acc k v) heap ""

let to_string sh =
  let str =  "Stack:\n" in
  let str = Stack.fold
    (fun k v acc -> Format.asprintf "%s\t%s -> %d\n" acc (SSL.Variable.show k) v)
    sh.stack
    str
  in
  let str = str ^ "\nHeap:\n" in
  let str =  str ^ heap_to_string sh.heap in
  let str = str ^ "\nFootprints:\n" in
  SSL.Map.fold
    (fun psi fp acc ->
      Format.asprintf "%s\t%a -> {%s}\n" acc SSL.pp psi (fp_to_string fp)
    ) sh.footprints str
  |> SSL.Map.fold
    (fun psi heap acc ->
      Format.asprintf "%s\nWitness heap of %a:\n%s" acc SSL.pp psi (heap_to_string heap)
    ) sh.heaps

let print sh =
  Format.printf "Stack:\n";
  Stack.iter (fun k v -> Format.printf "%s -> %d\n" (SSL.Variable.show k) v) sh.stack;
  Format.printf "Heap:\n";
  Heap.iter (fun k v -> Format.printf "%d -> %d\n" k v) sh.heap;
  Format.printf "Footprints:\n";
  SSL.Map.iter
    (fun psi fp -> Format.printf "%a -> {%s}\n" SSL.pp psi (fp_to_string fp)) sh.footprints

(* === Graph representation of (s,h) model === *)

module Vertex = struct

  include Int

  let hash = Hashtbl.hash

  let show loc = Format.asprintf "%d" loc

end

module HeapGraph = struct

  module Graph = Persistent.Digraph.ConcreteBidirectional(Vertex)
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

  let has_path g u v =
    let path_checker = PathChecker.create g in
    PathChecker.check_path path_checker u v

  let get_path g u v =
    try
      let path, _ = Dijkstra.shortest_path g u v in
      List.map E.src path

    (* If there is no path, return empty *)
    with Not_found -> []

  let model = ref None

  module Dot = Graphviz.Dot
    (struct
      include Graph

      let graph_attributes _ = []
      let default_vertex_attributes v = []
      let vertex_name v = Int.to_string v
      let vertex_attributes v =
        [`Label (Format.asprintf "%d : %s" v (String.concat "," (stack_inverse (Option.get !model) v)))]

      let get_subgraph _ = None
      let edge_attributes e = []
      let default_edge_attributes _ = []
    end)

end

let get_heap_graph sh =
  let heap = sh.heap in
  Heap.fold
    (fun source target g ->
       HeapGraph.add_edge g source target
    ) heap HeapGraph.empty

let get_path sh x y = HeapGraph.get_path (get_heap_graph sh) x y

let output_graph filename sh =
  let channel = open_out filename in
  HeapGraph.model := Some sh;
  HeapGraph.Dot.output_graph channel (get_heap_graph sh);
  close_out channel
