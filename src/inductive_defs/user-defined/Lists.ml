(** Singly-linked list parametrised by a sort S and field F. *)

open SL_builtins

module ID = InductiveDefinition
include ID

(** Singly-linked list *)
let ls =
  let header = SL.Variable.mk_list loc_ls ["x"; "y"] in
  let [x; y] = List.map SL.Term.of_var header in
  ID.mk "ls" header @@
    SL.mk_or [
      SL.mk_eq [x; y];
      SL.mk_exists' [loc_ls] (fun [n] ->
        SL.mk_star [
          SL.mk_distinct [x; y];
          SL.mk_pto x n;
          SL.mk_predicate "ls" [n; y]
    ])]

(** Singly-linked list of length 2+ *)
let ls_two_plus =
  let header = SL.Variable.mk_list loc_ls ["x"; "y"] in
  let [x; y] = List.map SL.Term.of_var header in
  ID.mk "ls_2plus" header @@
    SL.mk_exists' [loc_ls] (fun [n] ->
      SL.mk_star [
        SL.mk_pto x n;
        SL.mk_distinct [x; n; y];
        SL.mk_predicate "ls" [n; y];
    ])

(** Doubly-linked list *)
let dls =
  let header = SL.Variable.mk_list loc_dls ["x"; "y"; "xp"; "yp"] in
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

(** Three-parameter DLS *)
let dls_simple =
  let header = SL.Variable.mk_list loc_dls ["x"; "y"; "yp"] in
  let [x; y; y'] = List.map SL.Term.of_var header in
  ID.mk "dls_simple" header @@
    SL.mk_or [
      SL.mk_eq2 x y;
      SL.mk_exists' [SL_builtins.loc_dls] (fun [n] ->
        SL.mk_star [
          SL.mk_distinct2 x y;
          mk_pto_dls x ~next:n ~prev:y';
          SL.mk_predicate "dls_simple" [n; y; x]
    ])]

(** Three-parameter DLS *)
let dls_simple_two_plus =
  let header = SL.Variable.mk_list loc_dls ["x"; "y"; "yp"] in
  let [x; y; y'] = List.map SL.Term.of_var header in
  ID.mk "dls_simple_2plus" header @@
      SL.mk_exists' [SL_builtins.loc_dls] (fun [n] ->
        SL.mk_star [
          SL.mk_distinct [x; y; n];
          mk_pto_dls x ~next:n ~prev:y';
          SL.mk_predicate "dls_simple" [n; y; x]
      ])

(** Doubly-linked list of length 3+ *)
let dls_three_plus =
  let header = SL.Variable.mk_list loc_dls ["x"; "y"; "xp"; "yp"] in
  let [x; y; x'; y'] = List.map SL.Term.of_var header in
  ID.mk "dls_3plus" header @@
    SL.mk_exists' [loc_dls] (fun [n] ->
      SL.mk_star [
        mk_pto_dls x ~next:n ~prev:y';
        SL.mk_distinct2 x y;
        SL.mk_distinct2 x' y';
        SL.mk_distinct2 n y;
        SL.mk_distinct2 x x';
        SL.mk_predicate "dls" [n; y; x'; x];
    ])

(* Nested singly-linked list *)
let nls =
  let header = SL.Variable.mk_list loc_nls ["x"; "y"] @ [SL.Variable.mk "z" loc_ls] in
  let [x; y; z] = List.map SL.Term.of_var header in
  ID.mk "nls" header @@
    SL.mk_or [
      SL.mk_eq [x; y];
      SL.mk_exists' [loc_nls; loc_ls;] (fun [top; next] ->
        SL.mk_star [
          SL.mk_distinct [x; y];
          mk_pto_nls x ~top ~next;
          SL.mk_predicate "nls" [top; y; z];
          SL.mk_predicate "ls" [next; z];
    ])]

(* Nested singly-linked list of length 2+ *)
let nls_two_plus =
  let header = SL.Variable.mk_list loc_nls ["x"; "y"] @ [SL.Variable.mk "z" loc_ls] in
  let [x; y; z] = List.map SL.Term.of_var header in
  ID.mk "nls_2plus" header @@
    SL.mk_exists' [loc_nls; loc_ls] (fun [t; n] ->
      SL.mk_star [
        mk_pto_nls x ~top:t ~next:n;
        SL.mk_distinct [x; y; t];
        SL.mk_predicate "nls" [t; y; z];
        SL.mk_predicate "ls" [n; z];
    ])

(** Singly-linked list defined from backward *)
let ls_back =
  let header = SL.Variable.mk_list loc_ls ["x"; "y"] in
  let [x; y] = List.map SL.Term.of_var header in
  ID.mk "ls_back" header @@
    SL.mk_or [
      SL.mk_eq [x; y];
      SL.mk_exists' [loc_ls] (fun [p] ->
        SL.mk_star [
          SL.mk_distinct [p; y];
          SL.mk_pto p y;
          SL.mk_predicate "ls_back" [x; p]
    ])]
