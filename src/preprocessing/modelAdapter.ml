module Substitution = SL.Term.MonoMap(SL.Term)

type t = {
  substitution: Substitution.t;
  (** Substitutions that needs to be applied as part of the model adapter. *)

  fresh_vars: SL.Variable.Set.t;
  (** Variables that must be interpreted as fresh constants. *)

  skolem_vars: SL.Variable.Set.t;
  (** Variables that should be removed from model. *)
}

let empty = {
  substitution = Substitution.empty;
  fresh_vars = SL.Variable.Set.empty;
  skolem_vars = SL.Variable.Set.empty;
}

let pp fmt self =
  Format.fprintf fmt "Model adapter: {@,@[<v 4>substitution: %s@,fresh variables: %s@,skolem variables: %s@,@]}"
    (Substitution.show self.substitution)
    (SL.Variable.Set.show self.fresh_vars)
    (SL.Variable.Set.show self.skolem_vars)

let show = Format.asprintf "%a" pp

let add_substitution self target by = {self with substitution = Substitution.add target by self.substitution}

let add_fresh_vars self vars =
  {self with fresh_vars = SL.Variable.Set.union vars self.fresh_vars}

let add_skolem_vars self vars =
  {self with skolem_vars = SL.Variable.Set.union vars self.skolem_vars}

let apply_fresh adapter model = failwith "not implemented"

let apply_substitutions adapter model = failwith "not implemented"

let apply_skolem adapter model =
  StackHeapModel.filter_vars (fun var ->
    not @@ SL.Variable.Set.mem var adapter.skolem_vars
  ) model

let apply adapter model =
  apply_fresh adapter model
  |> apply_skolem adapter
  |> apply_substitutions adapter
