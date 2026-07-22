open MemoryModel

[@@@warning "+8"]

type t =
  | Constant of Constant.t
  (* Equality *)
  | Equal | Distinct
  (* Propositional Logic *)
  | And | Or | Not | Implies | Iff | IfThenElse
  (* Arithmetic *)
  | Plus | Minus | Mult | Lesser | LesserEqual
  (* Bitvectors *)
  | BitPlus of Int.t
  | BitCheck | BitNot | BitAnd of Int.t | BitOr of Int.t | BitXor of Int.t
  | BitUnsignedLesser | BitUnsignedLesserEqual
  | BitImplies | BitCompl | BitShiftLeft | BitShiftRight
  (* Sets *)
  | Membership | Subset | Disjoint | Union of Sort.t | Inter of Sort.t | Diff | Compl
  | Enum of Sort.t | Universe of Sort.t
  (* Arrays *)
  | ConstArray of Sort.t | Select | Store
  | Cast of Sort.t
  (* Separation logic *)
  | Constructor of StructDef.t
  | Emp | Pure | PointsTo
  | Predicate of Identifier.t * (StructDef.t List.t)
  | HeapTerm of MemoryModel0.Field.t | BlockBegin | BlockEnd
  | GuardedNot | Star | Septraction
  [@@ deriving equal, compare]

(** TODO: So far only used for tests. Proper implementation can be used
          for comparison. *)
let can_reorder = function
  | Constant _ | Equal | And | Or | Iff | Star | Union _ | Inter _ -> true
  | _ -> false

let show = function
  | Constant c -> Constant.show c
  | Equal -> "eq" | Distinct -> "distinct"
  | And -> "and" | Or -> "or" | Not -> "not" | Implies -> "->"
  | Iff -> "<->" | IfThenElse -> "ite"

  | Lesser | BitUnsignedLesser -> "<"
  | LesserEqual | BitUnsignedLesserEqual -> "<="

  | Plus -> "+" | Minus -> "-" | Mult -> "*"
  | BitPlus _ -> "bvadd"
  | BitCheck -> "bit-check"
  | BitNot -> "bit-not"
  | BitAnd _ -> "bit-and"
  | BitOr _ -> "bit-or"
  | BitXor _ -> "bit-xor"
  | BitImplies -> "bit-implies" | BitCompl -> "bit-compl"
  | BitShiftLeft -> ">>" | BitShiftRight -> "<<"

  | Membership -> "mem" | Subset -> "subset" | Disjoint -> "disjoint"
  | Union _ -> "union" | Inter _ -> "inter" | Diff -> "diff" | Compl -> "compl"
  | Enum _ -> "set" | Universe _ -> "universe"
  | ConstArray _ -> "const-arr" | Select -> "select" | Store -> "store"
  | Cast sort -> Format.asprintf "2%s" (Sort.show sort)

  | Pure -> "pure"
  | Emp -> "emp"
  | HeapTerm field -> Format.asprintf "%s" (MemoryModel0.Field.show field)
  | Constructor def -> StructDef.show def
  | PointsTo -> "pto"
  | Predicate (id, _) -> Identifier.show id
  | BlockBegin -> "begin"
  | BlockEnd -> "end"

  | GuardedNot -> "gneg"
  | Star -> "star"
  | Septraction -> "septraction"

type show_kind =
  | Prefix
  | Infix

let show_kind = function
  | HeapTerm _ -> Infix
  | _ -> Prefix

let get_sort app xs = match app with
  | Constant c -> Constant.get_sort c
  | And | Or | Not | Implies | Iff | Equal | Distinct | Lesser | LesserEqual -> Sort.bool
  | Pure | Emp | PointsTo | Predicate _ | Star | Septraction | GuardedNot -> Sort.bool
  | Membership | Subset | Disjoint -> Sort.bool
  | BitCheck | BitUnsignedLesser | BitUnsignedLesserEqual -> Sort.bool
  | Plus | Minus | Mult -> Sort.int
  | Union sort | Inter sort | Enum sort | Universe sort -> sort
  | Diff | Compl -> List.hd xs
  | BitPlus width | BitAnd width | BitOr width | BitXor width -> Sort.mk_bitvector width
  | BitNot | BitShiftLeft | BitShiftRight | BitImplies | BitCompl -> List.hd xs
  | HeapTerm (field) -> Field.get_sort field
  | IfThenElse -> List.nth xs 1
  | ConstArray sort -> List.nth xs 0
  | Select -> Sort.get_range_sort @@ List.nth xs 0
  | Store -> List.hd xs
  | Cast sort -> sort
  | BlockBegin | BlockEnd -> List.hd xs

  | Constructor def -> failwith "\"constructor\" should not be accessed as a standalone term"
