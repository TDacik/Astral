

type width = Int.t [@@warning "-34"]

module type CONFIG = sig
  val width : width
end

module Make(C : CONFIG) () = struct

  (** Sort used for address variables *)
  let sort = Sort.mk_bitvector C.width

  module Variable = struct
    module V = Variable.Make()

    type t = V.t

    let mk width name = V.mk name (Sort.mk_bitvector width)

    let mk_fresh width name = V.mk_fresh name (Sort.mk_bitvector width)

    let mk_ptr name = V.mk name sort

    let mk_fresh_ptr width name = V.mk_fresh name sort

    let get_name = V.get_name

    let get_width v = Sort.get_width @@ V.get_sort v

    let get_sort v = V.get_sort v

    let show = V.show

  end

  module Operation = struct

    type t =
      | Plus
      | Minus
      | Mult
      (* TODO ... *)

    let arity = function
      | Plus | Mult -> None
      | Minus -> Some 2

    let get_width = function
      | Plus | Minus | Mult -> List.hd

    let show = function
      | Plus -> "+"
      | Minus -> "-"
      | Mult -> "*"

  end

  module Term = struct

    type t =
      | Var of Variable.t
      | Const of Bitvector.t
      | BlockBegin of t
      | BlockEnd of t
      | Application of Operation.t * t list

    let rec get_width = function
      | Var v -> Variable.get_width v
      | Const (_, width) -> width
      | BlockBegin t | BlockEnd t -> get_width t
      | Application (app, args) -> Operation.get_width app @@ List.map get_width args

    let null = Const (Bitvector.of_int 0 C.width)

    let mk_var size name = Var (Variable.mk size name)

    let mk_fresh_var size name = Var (Variable.mk_fresh size name)

    let mk_ptr_var name = mk_var C.width name

    let mk_fresh_ptr_var name = mk_fresh_var C.width name

    let mk_const ~size value = Const (Bitvector.of_int value size)

    let mk_block_begin t = BlockBegin t

    let mk_block_end t = BlockEnd t

    let mk_plus x y = Application (Plus, [x; y])

    let mk_minus x y = Application (Minus, [x; y])

    let mk_mult x y = Application (Mult, [x; y])

    let rec collect_vars = function
      | Var v -> [v]
      | Const _ -> []
      | BlockBegin t | BlockEnd t -> collect_vars t
      | Application (app, args) -> List.concat_map collect_vars args

    let rec collect_block_terms = function
      | Var _ | Const _ -> []
      | BlockBegin t | BlockEnd t -> t :: collect_block_terms t
      | Application (app, args) -> List.concat_map collect_block_terms args

    let rec show = function
      | Var var -> Variable.show var
      | Const c -> Bitvector.show c
      | BlockBegin x -> Format.asprintf "begin(%s)" @@ show x
      | BlockEnd x -> Format.asprintf "end(%s)" @@ show x
      | Application (app, args) ->
        Format.asprintf "%s(%s)"
          (Operation.show app)
          (String.concat ", " @@ List.map show args)

  end

  open Term

  type t =
    | Emp
    | True
    | Eq of Term.t list
    | Distinct of Term.t list
    | Lesser of Term.t * Term.t
    | LesserEqual of Term.t * Term.t
    | PointsTo of Term.t * Term.t
    | PointsToArr of Term.t * Bitvector.t option * Term.t
    | Star of t list
    | Exists of Variable.t list * t

  let rec show phi =
    let aux name (args : 'a list) (show_fn : 'a -> string) =
      Format.asprintf "%s(%s)" name (String.concat ", " @@ List.map show_fn args)
    in
    match phi with
    | Emp -> "emp"
    | True -> "true"
    | Eq [x; y] -> Format.asprintf "%s = %s" (Term.show x) (Term.show y)
    | Distinct [x; y] -> Format.asprintf "%s != %s" (Term.show x) (Term.show y)
    | Eq xs -> aux "eq" xs Term.show
    | Distinct xs -> aux "distinct" xs Term.show
    | Lesser (x, y) -> Format.asprintf "%s < %s" (Term.show x) (Term.show y)
    | LesserEqual (x, y) -> Format.asprintf "%s <= %s" (Term.show x) (Term.show y)
    | PointsTo (x, y) -> Format.asprintf "%s -> %s" (Term.show x) (Term.show y)
    | PointsToArr (x, c, size) ->
      let c_str = match c with None -> "?" | Some b -> Bitvector.show b in
      Format.asprintf "%s -> %s[%s]" (Term.show x) c_str (Term.show size)
    | Star xs -> String.concat " * " @@ List.map show xs
    | _ -> "TODO"

  (** Constructors *)

  exception SortError of string

  let assert_same_width = function
    | [] -> ()
    | x :: xs ->
      let w = get_width x in
      if List.for_all (fun t -> Int.equal w (get_width t)) xs then ()
      else raise @@ SortError "Width missmatch"

  let emp = Emp

  let tt = True

  let mk_eq terms =
    assert_same_width terms;
    Eq terms

  let mk_eq2 x y = mk_eq [x; y]

  let mk_distinct terms =
    assert_same_width terms;
    Distinct terms

  let mk_distinct2 x y = mk_distinct [x; y]

  let mk_lesser x y = Lesser (x, y)

  let mk_lesser_or_eq x y = LesserEqual (x, y)

  let mk_greater x y = mk_lesser y x

  let mk_greater_or_eq x y = mk_lesser_or_eq y x

  let mk_pto x y = PointsTo (x, y)

  let mk_pto_array ?const ~size x = PointsToArr (x, const, size)

  let mk_star psis = Star psis

  let mk_exists xs psi = Exists (xs, psi)

  (** Translation *)

  module Context = struct

    type t = {
      null : SMT.t;
      begin_arr : SMT.t;
      end_arr : SMT.t;
    }

    let init () =
      let loc_sort = Sort.mk_bitvector C.width in
      let arr_sort = Sort.mk_array loc_sort loc_sort in
      {
        null = SMT.Bitvector.mk_const_of_int 0 C.width;
        begin_arr = SMT.Array.mk_var "__arr_begin" arr_sort;
        end_arr = SMT.Array.mk_var "__arr_end" arr_sort;
      }

  end

  open Context

  let translate_var v =
    SMT.mk_var (Variable.get_name v) (Variable.get_sort v)

  let rec translate_term ctx = function
    | Var v -> translate_var v
    | Const c -> SMT.Bitvector.mk_const c
    | BlockBegin t -> SMT.Array.mk_select ctx.begin_arr (translate_term ctx t)
    | BlockEnd t -> SMT.Array.mk_select ctx.end_arr (translate_term ctx t)
    | Application (Plus, xs) ->
      SMT.Bitvector.mk_plus (get_width @@ List.hd xs) @@ List.map (translate_term ctx) xs
    | Application (Mult, xs) ->
      SMT.Bitvector.mk_mult (get_width @@ List.hd xs) @@ List.map (translate_term ctx) xs
    | Application (Minus, [x; y]) ->
      (* TODO: we may want to use mk_minus which is transformed later *)
      SMT.Bitvector.mk_plus (get_width x) [
        (translate_term ctx x);
        SMT.Bitvector.mk_not (translate_term ctx y);
      ]

  let rec translate ctx = function
    | Emp -> SMT.Boolean.tt
    | True -> SMT.Boolean.tt
    | Eq terms -> SMT.Boolean.mk_eq @@ List.map (translate_term ctx) terms
    | Distinct terms -> SMT.Boolean.mk_distinct @@ List.map (translate_term ctx) terms
    | Lesser (x, y) -> SMT.Bitvector.mk_lesser (translate_term ctx x) (translate_term ctx y)
    | LesserEqual (x, y) -> SMT.Bitvector.mk_lesser_eq (translate_term ctx x) (translate_term ctx y)
    | PointsTo (source, target) -> SMT.Boolean.tt
    | PointsToArr (source, const, size) -> SMT.Boolean.tt
    | Star psis -> SMT.Boolean.mk_and @@ List.map (translate ctx) psis
    | Exists (_, psi) -> translate ctx psi

  type allocation =
    | Singleton of SMT.t
    | Range of SMT.t * SMT.t

  let rec collect_ptos ctx = function
    | Emp | True | Eq _ | Distinct _ | Lesser _ | LesserEqual _ -> []
    | PointsTo (source, _) -> [Singleton (translate_term ctx source)]
    | PointsToArr (source, _, size) -> [Range (translate_term ctx source, translate_term ctx size)]
    | Exists (_, psi) -> collect_ptos ctx psi
    | Star psis -> List.concat_map (collect_ptos ctx) psis

  let rec collect_block_terms = function
    | Emp | True -> []
    | Eq terms | Distinct terms -> List.concat_map Term.collect_block_terms terms
    | PointsTo (x, y) | Lesser (x, y) | LesserEqual (x, y) ->
      List.concat_map Term.collect_block_terms [x; y]
    | PointsToArr (source, _, _) -> Term.collect_block_terms source
    | Exists (_, psi) -> collect_block_terms psi
    | Star psis -> List.concat_map collect_block_terms psis

  (** Generate axiom that allocation if valid. *)
  let pto_valid_axiom ctx = function
    | Singleton t -> SMT.Boolean.mk_distinct [t; ctx.null]
    | Range (start, size) ->
      let sum = SMT.Bitvector.mk_plus C.width [start; size] in
      SMT.Boolean.mk_and [
        SMT.Bitvector.mk_lesser_eq start sum; (* No overflow 1 *)
        SMT.Bitvector.mk_lesser_eq size sum;  (* No overflow 2 *)
      ]

  (** Generate axioms that two allocated regions are disjoint. *)
  let pto_disjoint_axiom ctx (x, y) = match x, y with
    | Singleton t1, Singleton t2 ->
      SMT.Boolean.mk_distinct [t1; t2]
    | Singleton t, Range (start, size) | Range (start, size), Singleton t ->
      let max = SMT.Bitvector.mk_plus C.width [start; size] in
      SMT.Boolean.mk_or [
        SMT.Bitvector.mk_lesser t start;
        SMT.Bitvector.mk_lesser_eq max t;
      ]
    | Range (start1, size1), Range (start2, size2) ->
      let max1 = SMT.Bitvector.mk_plus C.width [start1; size1] in
      let max2 = SMT.Bitvector.mk_plus C.width [start2; size2] in
      SMT.Boolean.mk_or [
        SMT.Bitvector.mk_lesser_eq max1 start2;
        SMT.Bitvector.mk_lesser_eq max2 start1;
      ]

  let pto_axioms ctx phi =
    let ptos = collect_ptos ctx phi in
    let valid = List.map (pto_valid_axiom ctx) ptos in
    let disjoint =
      List_utils.diagonal_product ptos
      |> List.map (pto_disjoint_axiom ctx)
    in
    SMT.Boolean.mk_and (valid @ disjoint)

  (** Memory model axioms *)

  let blocks_have_positive_size ctx terms =
    let non_null t =
      SMT.Bitvector.mk_lesser
        (SMT.Array.mk_select ctx.begin_arr t)
        (SMT.Array.mk_select ctx.end_arr t)
    in
    let generator t =
      SMT.Boolean.mk_or [
        non_null t;
        SMT.Boolean.mk_eq [
          t;
          SMT.Array.mk_select ctx.begin_arr t;
          SMT.Array.mk_select ctx.end_arr t;
          ctx.null;
        ]
      ]
    in
    List.map generator terms
    |> SMT.Boolean.mk_and

  let blocks_do_not_overlap ctx terms =
    let generator (t1, t2) =
      let b1 = SMT.Array.mk_select ctx.begin_arr t1 in
      let e1 = SMT.Array.mk_select ctx.end_arr t1 in
      let b2 = SMT.Array.mk_select ctx.begin_arr t2 in
      let e2 = SMT.Array.mk_select ctx.end_arr t2 in
      SMT.Boolean.mk_or [
        (* Blocks are same *)
        SMT.Boolean.mk_and [
          SMT.Boolean.mk_eq [b1; b2];
          SMT.Boolean.mk_eq [e1; e2];
        ];
        (* Blocks do not overlap 1 *)
        SMT.Bitvector.mk_lesser_eq e1 b2;
        (* Blocks do not overlap 2 *)
        SMT.Bitvector.mk_lesser_eq e2 b1;
      ]
    in
    List_utils.diagonal_product terms
    |> List.map generator
    |> SMT.Boolean.mk_and


  let translate_all ctx phi =
    let block_terms = List.map (translate_term ctx) @@ collect_block_terms phi in
    SMT.Boolean.mk_and [
      translate ctx phi;
      pto_axioms ctx phi;
      blocks_have_positive_size ctx block_terms;
      blocks_do_not_overlap ctx block_terms;
    ]

  let check_sat phi =
    let module Backend = BitwuzlaNative.Init () in
    let ctx = Context.init () in
    let query = translate_all ctx phi in
    match Backend.check_sat query with
    | SMT_Sat _ -> `Sat
    | SMT_Unsat _ -> `Unsat
    | SMT_Unknown _ -> `Unknown

end
