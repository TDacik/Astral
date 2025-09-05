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
  ID.mk "ls_2_plus" header @@
    SL.mk_exists' [loc_ls; loc_ls] (fun [n; m] ->
      SL.mk_star [
        SL.mk_pto x n;
        SL.mk_pto n m;
        SL.mk_distinct [x; n; m; y];
        SL.mk_predicate "ls" [m; y];
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

(** Doubly-linked list of length 3+ *)
let dls_three_plus =
  let header = SL.Variable.mk_list loc_dls ["x"; "y"; "xp"; "yp"] in
  let [x; y; x'; y'] = List.map SL.Term.of_var header in
  ID.mk "dls_3_plus" header @@
    SL.mk_exists' [loc_dls; loc_dls; loc_dls] (fun [n1; n2; n3] ->
      SL.mk_star [
        mk_pto_dls x ~next:n1 ~prev:y';
        mk_pto_dls n1 ~next:n2 ~prev:x;
        mk_pto_dls n2 ~next:n3 ~prev:n1;
        SL.mk_distinct [x; n1; n2; n3; y];
        SL.mk_predicate "dls" [n3; y; x'; n2];
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

(* Nested singly-linked list *)
let nls_one_plus =
  let header = SL.Variable.mk_list loc_nls ["x"; "y"] @ [SL.Variable.mk "z" loc_ls] in
  let [x; y; z] = List.map SL.Term.of_var header in
  ID.mk "nls_one_plus" header @@
    SL.mk_or [
      mk_pto_nls x ~top:y ~next:z;
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
  ID.mk "nls_two_plus" header @@
    SL.mk_exists' [loc_nls; loc_nls; loc_ls; loc_ls] (fun [t1; t2; n1; n2] ->
      SL.mk_star [
        mk_pto_nls x ~top:t1 ~next:n1;
        mk_pto_nls t1 ~top:t2 ~next:n2;
        SL.mk_distinct [x; t1; t2; y];
        SL.mk_distinct [n1; n2; z];
        SL.mk_predicate "ls" [n1; z];
        SL.mk_predicate "ls" [n2; z];
        SL.mk_predicate "nls" [t2; y; z];
    ])
