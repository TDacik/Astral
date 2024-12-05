open Testable_sig

module Make (T : TESTABLE_BASE) = struct

  type t = T.t

  module T = struct
    include T
    let show_list xs = "[" ^ (String.concat ", " @@ List.map show xs) ^ "]"
  end

  (** Testable as required by alcotest *)
  module T' = struct
    include T
    let pp fmt x = Format.fprintf fmt "%s" (T.show x)
  end

  let check_equal actual expected =
    Alcotest.check' (module T' : TESTABLE with type t = t) ~msg:"" ~actual ~expected

  let check_apply ~input ~expected fn =
    let msg = Format.asprintf "Input: %s" (T.show input) in
    Alcotest.check' (module T' : TESTABLE with type t = t) ~msg ~actual:(fn input) ~expected

  let check (prop : t -> bool) x =
    let msg = Format.asprintf "Property is false for %s" (T.show x) in
    Alcotest.check' Alcotest.bool ~msg ~actual:(prop x) ~expected:true

  let check2 (prop : t -> t -> bool) lhs rhs =
    let msg = Format.asprintf "Property is false for %s and %s" (T.show lhs) (T.show rhs) in
    Alcotest.check' Alcotest.bool ~msg ~actual:(prop lhs rhs) ~expected:true

  let check_list (prop : t list -> bool) xs =
    let msg = Format.asprintf "Property is false for %s" (T.show_list xs) in
    Alcotest.check' Alcotest.bool ~msg ~actual:(prop xs) ~expected:true

  let check_not_list (prop : t list -> bool) xs =
    let msg = Format.asprintf "Property is true for %s" (T.show_list xs) in
    Alcotest.check' Alcotest.bool ~msg ~actual:(prop xs) ~expected:false


  let check_not2 (prop : t -> t -> bool) lhs rhs =
    let msg = Format.asprintf "Property is true (expected false) for %s and %s" (T.show lhs) (T.show rhs) in
    Alcotest.check' Alcotest.bool ~msg ~actual:(prop lhs rhs) ~expected:false

  let check_not (prop : t -> bool) x =
    let msg = Format.asprintf "Property is true (expected false) for %s" (T.show x) in
    Alcotest.check' Alcotest.bool ~msg ~actual:(prop x) ~expected:false

  let check_int (compute : t -> int) x expected =
    let actual = compute x in
    let msg = Format.asprintf "Result for %s is %d (expected %d)" (T.show x) actual expected in
    Alcotest.check' Alcotest.int ~msg ~actual ~expected

end
