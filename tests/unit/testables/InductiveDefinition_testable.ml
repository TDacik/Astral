open SL_testable
open SL_builtins
open MemoryModel

module ID = InductiveDefinition
include ID

(** Definitions *)

let ls_def x y =
  SL.mk_or [
    SL.mk_eq [x; y];
    SL.mk_exists' [loc_ls] (fun [n] ->
      SL.mk_star [
        SL.mk_distinct [x; y];
        SL.mk_pto x n;
        SL.mk_predicate "ls" [n; y]
  ])]

let ls = ID.mk "ls" [Var.x; Var.y] @@ ls_def x y

let dls =
  let header = SL.Variable.mk_list loc_dls ["x"; "y"; "x'"; "y'"] in
  let [x; y; x'; y'] = List.map SL.Term.of_var header in
  ID.mk "dls" header @@
    SL.mk_or [
      SL.mk_and [SL.mk_eq2 x y; SL.mk_eq2 x' y'];
      SL.mk_exists' [SL_builtins.loc_dls] (fun [n] ->
        SL.mk_star [
          SL.mk_distinct2 x y;
          SL.mk_distinct2 x' y';
          mk_pto_dls x ~next:n ~prev:y';
          SL.mk_predicate "dls" [n; y; x'; x]
    ])]

let dnls =
  let sort = Sort.mk_loc "RefDNLS" in
  let def = StructDef.mk "DNLS" [Field.mk "next" sort; Field.mk "prev" sort; Field.mk "down" loc_ls] in
  let header = SL.Variable.mk_list sort ["x"; "y"; "x'"; "y'"; "z"] in
  let [x; y; x'; y'; z] = List.map SL.Term.of_var header in
  ID.mk "dnls" header @@
    SL.mk_or [
      SL.mk_and [SL.mk_eq2 x y; SL.mk_eq2 x' y'];
      SL.mk_exists' [loc_dls; loc_ls] (fun [n; d] ->
        SL.mk_star [
          SL.mk_distinct2 x y;
          SL.mk_distinct2 x' y';
          mk_pto_struct x def [n; y'; d];
          SL.mk_predicate "dnls" [n; y; x'; x; z];
          SL.mk_predicate "ls" [d; z]
    ])]

(** ==== Tree with linked leaves ==== *)

module TLL = struct

  let sort = Sort.mk_loc "RefTLL"

  let def = StructDef.mk "TLL" [Field.mk "next" sort; Field.mk "left" sort; Field.mk "right" sort]

  let id =
    let header = SL.Variable.mk_list sort ["x"; "ll"; "lr"] in
    let [x; ll; lr] = List.map SL.Term.of_var header in
    let nil = SL.Term.nil in
    ID.mk "tll" header @@
      SL.mk_or [
        SL.mk_star [
          SL.mk_pto_struct x def [lr; nil; nil];
          SL.mk_eq [x; ll]
        ];
        SL.mk_exists' [sort; sort; sort] (fun [l; r; mid] ->
          SL.mk_star [
            SL.mk_pto_struct x def [l; r; nil];
            SL.mk_predicate "tll" [l; ll; mid];
            SL.mk_predicate "tll" [r; mid; lr];
          ]
        )
      ]
end
