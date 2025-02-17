open SL

type t = {
  eqs : (Variable.t * Variable.t) list;
  skolem_variables: Variable.t list;
}

let empty = {
  eqs = [];
  skolem_variables = [];
}

let show self =
  Format.asprintf "Model adapter: {\n  Skolem variables %s,\n}" (SL.Variable.show_list self.skolem_variables)

let add_skolem_var self var = {self with skolem_variables = var :: self.skolem_variables}

let apply adapter model =
  StackHeapModel.filter_vars (fun var ->
    not @@ BatList.mem_cmp Variable.compare var adapter.skolem_variables
  ) model
