open ID_sig
open Result_syntax

module F = Format

module Make (B : BUILTIN_BASE) = struct

  include B

  let instantiate heap_sort arguments =
    if List.length arguments != B.arity then
      Result.Error (F.sprintf "incorrect arity (expected %d)" B.arity)

    else if SL.Term.is_nil @@ List.hd arguments then
      let nil_case = Option.get @@ B.preprocess SL_graph.empty arguments in
      Result.Ok nil_case

    else
      let* structs = B.instantiate heap_sort (List.map SL.Term.get_sort arguments) in
      Result.Ok (SL.mk_predicate B.name arguments ~structs)

end
