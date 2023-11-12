(* Tests for operations over SSL formulae
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL
open SSL.Infix

let nil = SSL.mk_nil ()
let p1 = SSL.mk_pure (SMT.Arithmetic.mk_var "p1")
let p2 = SSL.mk_pure (SMT.Arithmetic.mk_var "p2")

let x = SSL.mk_var "x" Sort.loc_ls
let y = SSL.mk_var "y" Sort.loc_ls
let z = SSL.mk_var "z" Sort.loc_ls

let var_list_eq vars1 vars2 =
  let vars1 = List.sort SSL.Variable.compare vars1 in
  let vars2 = List.sort SSL.Variable.compare vars2 in
  List.equal SSL.Variable.equal vars1 vars2

(** Tests *)

let get_vars_test1 () =
  let phi = SSL.mk_eq nil nil in
  assert (var_list_eq (get_vars ~with_nil:false phi) []);
  assert (var_list_eq (get_vars ~with_nil:true phi) [SSL.Variable.nil])

let get_vars_test2 () =
  let phi = x |-> y in
  let x = SSL.Variable.mk "x" Sort.loc_ls in
  let y = SSL.Variable.mk "y" Sort.loc_ls in
  assert (var_list_eq (get_vars ~with_nil:false phi) [x; y]);
  assert (var_list_eq (get_vars ~with_nil:true phi) [x; y])


let rename_var_test1 () =
  let phi = y |-> nil in
  assert (rename_var phi "x" "z" === phi)

let rename_var_test2 () =
  let phi = x |-> y in
  let phi' = z |-> y in
  assert (rename_var phi "x" "z" === phi')


let is_precise () =
  assert (is_pure (p1 == p2));
  assert (is_pure (p2 != p1))

let is_pure () =
  assert (is_pure @@ SSL.mk_pure @@ SMT.Boolean.mk_false ());
  assert (is_pure @@ SSL.mk_pure @@ SMT.Boolean.mk_true ());
  assert (is_pure @@ SSL.mk_eq (SSL.mk_nil ()) (SSL.mk_nil ()));
  assert (not @@ is_pure @@ SSL.mk_emp ());
  assert (not @@ is_pure @@ SSL.mk_not @@ SSL.mk_emp ())


let is_positive_test1 () =
  assert (is_positive (x |-> y))

let is_positive_test2 () =
  let phi = (x |-> y) * (x == y) in
  assert (is_positive phi)

let is_positive_test3 () =
  let phi = (x |-> y) &! (y |-> x) in
  assert (is_positive phi)

let is_positive_test4 () =
  let phi = SSL.mk_not (y |-> x) in
  assert (not @@ is_positive phi)

let is_true1 () = assert (is_true @@ mk_true ())
let is_true2 () = assert (is_true @@ mk_not @@ mk_false ())

let is_false1 () = assert (is_false @@ mk_false ())
let is_false2 () = assert (is_false @@ mk_not @@ mk_true ())

let is_emp_test1 () = assert (is_emp @@ mk_emp ())
let is_emp_test2 () = assert (not @@ is_emp @@ mk_not @@ mk_emp ())

let subformula_id_test1 () =
  let phi = x |-> y in
  assert (SSL.subformula_id phi phi = 0)

let subformula_id_test2 () =
  let psi1 = x |-> y in
  let psi2 = y |-> x in
  let phi = psi1 * psi2 in
  assert (SSL.subformula_id phi phi = 0);
  assert (SSL.subformula_id phi psi1 = 1);
  assert (SSL.subformula_id phi psi2 = 2)

let subformula_id_test2 () =
  let psi2 = z |-> z in
  let psi3 = y |-> x in
  let psi1 = psi2 * psi3 in
  let psi5 = x |-> z in
  let psi6 = y |-> y in
  let psi4 = psi5 &! psi6 in
  let phi = psi1 * psi4 in
  assert (SSL.subformula_id phi phi = 0);
  assert (SSL.subformula_id phi psi1 = 1);
  assert (SSL.subformula_id phi psi2 = 2);
  assert (SSL.subformula_id phi psi3 = 3);
  assert (SSL.subformula_id phi psi4 = 4);
  assert (SSL.subformula_id phi psi5 = 5);
  assert (SSL.subformula_id phi psi6 = 6)

(** Fragment classification *)

let is_symbolic_heap_test1 () =
  let phi = x |-> y in
  assert (SSL.is_symbolic_heap phi)

let is_symbolic_heap_test2 () =
  let phi = (x |-> y) * (y |~> x) in
  assert (SSL.is_symbolic_heap phi)

let is_symbolic_heap_test3 () =
  let phi = SSL.mk_true () in
  assert (not @@ SSL.is_symbolic_heap phi)

let is_symbolic_heap_test4 () =
  let phi = x |-> y && x == y in
  assert (not @@ SSL.is_symbolic_heap phi)

let is_symbolic_heap_test5 () =
  let phi = SSL.mk_exists [x; y] (x |-> y) in
  assert (SSL.is_symbolic_heap phi)

let is_symbolic_heap_entl_test1 () =
  assert (SSL.is_symbolic_heap_entl @@ SSL.mk_false ())

let is_symbolic_heap_entl_test2 () =
  let lhs = x |-> y in
  let rhs = SSL.mk_exists [x; y] lhs in
  let phi = SSL.mk_gneg lhs rhs in
  assert (SSL.is_symbolic_heap_entl phi)

(** Views at SSL formulae *)

let as_symbolic_heap_test1 () =
  let pure = x == y in
  let spatial = x |-> y in
  let phi = spatial * pure in
  let pure', spatial' = SSL.as_symbolic_heap phi in
  assert (List.hd pure' === pure);
  assert (List.hd spatial' === spatial)

let as_symbolic_heap_test2 () =
  let phi = x |-> y in
  let pure, spatial = SSL.as_symbolic_heap phi in
  assert (pure = []);
  assert (List.hd spatial === phi)

let as_symbolic_heap_test3 () =
  let phi = x == y in
  let pure, spatial = SSL.as_symbolic_heap phi in
  assert (spatial = []);
  assert (List.hd pure === phi)

(* TODO: check *)
let as_symbolic_heap_test4 () =
  let phi = SSL.mk_emp () in
  let pure, spatial = SSL.as_symbolic_heap phi in
  assert (spatial = []);
  assert (List.hd pure === phi)


(* ==== Subformulae manipulation ==== *)

let positive_polarity_test1 () =
  let phi = (x |-> y) * (y |-> x) in
  let psi = (x |-> y) * (y |-> x) in
  assert (positive_polarity phi psi)

let positive_polarity_test2 () =
  let phi = (x |-> y) * (y |-> x) in
  let psi1 = x |-> y in
  let psi2 = y |-> x in
  assert (positive_polarity phi psi1);
  assert (positive_polarity phi psi2)

let positive_polarity_test3 () =
  let phi = SSL.mk_not @@ (x |-> y) * (y |-> x) in
  let psi1 = x |-> y in
  let psi2 = y |-> x in
  assert (not @@ positive_polarity phi psi1);
  assert (not @@ positive_polarity phi psi2)

let positive_polarity_test4 () =
  let phi = SSL.mk_gneg (x |-> y) ((x |-> y) * (y |-> x)) in
  let psi1 = x |-> y in
  let psi2 = y |-> x in
  assert (positive_polarity phi psi1);
  assert (not @@ positive_polarity phi psi2)

(* Assert : fail*)
let positive_polarity_test5 () =
  let phi = (x |-> y) * (y |-> x) in
  let psi = x |-> x in
  assert (not @@ positive_polarity phi psi)



let () =
  run "SSL" [
    "get_vars", [
      test_case "nil = nil" `Quick get_vars_test1;
      test_case "x |-> y"   `Quick get_vars_test2;
    ];
    "rename_var", [
      test_case "Test" `Quick rename_var_test1;
      test_case "Test" `Quick rename_var_test2;
    ];
    "is_precise", [
      test_case "Test"  `Quick is_precise;
    ];
    "is_pure", [
      test_case "Test"  `Quick is_pure;
    ];
    "is_true", [
      test_case "Test"  `Quick is_true1;
      test_case "Test"  `Quick is_true2;
    ];
    "is_false", [
      test_case "Test"  `Quick is_false1;
      test_case "Test"  `Quick is_false2;
    ];
    "is_emp", [
      test_case "Test"  `Quick is_emp_test1;
      test_case "Test"  `Quick is_emp_test2;
    ];
    "is_positive", [
      test_case "Test"  `Quick is_positive_test1;
      test_case "Test"  `Quick is_positive_test2;
      test_case "Test"  `Quick is_positive_test3;
      test_case "Test"  `Quick is_positive_test4;
    ];
    "subformula_id", [
      test_case "Test"  `Quick subformula_id_test1;
      test_case "Test"  `Quick subformula_id_test2;
    ];
    "positive_polarity", [
      test_case "Test"  `Quick positive_polarity_test1;
      test_case "Test"  `Quick positive_polarity_test2;
      test_case "Test"  `Quick positive_polarity_test3;
      test_case "Test"  `Quick positive_polarity_test4;
      test_case "Test"  `Quick positive_polarity_test5;
    ];
    "is_symbolic_heap", [
      test_case "Test"  `Quick is_symbolic_heap_test1;
      test_case "Test"  `Quick is_symbolic_heap_test2;
      test_case "Test"  `Quick is_symbolic_heap_test3;
      test_case "Test"  `Quick is_symbolic_heap_test4;
      test_case "Test"  `Quick is_symbolic_heap_test5;
    ];
    "is_symbolic_heap_entl", [
      test_case "Test"  `Quick is_symbolic_heap_entl_test1;
      test_case "Test"  `Quick is_symbolic_heap_entl_test2;
    ];
    "as_symbolic_heap", [
      test_case "Test"  `Quick as_symbolic_heap_test1;
      test_case "Test"  `Quick as_symbolic_heap_test2;
      test_case "Test"  `Quick as_symbolic_heap_test3;
      test_case "Test"  `Quick as_symbolic_heap_test4;
    ];
  ]
