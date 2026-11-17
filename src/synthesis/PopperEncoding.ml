open MemoryModel

(* TODO: do proper name mangling with lookup in hashtable *)
let field field =
  Format.asprintf "%sX%s" (Field.get_name field) (Sort.name @@ Field.get_sort field)
  |> String.uncapitalize_ascii

let decode_field_aux str =
  BatString.split_on_char 'X' str
  |> List.hd

let decode_field ctx str =
  BatString.split_on_char 'X' str
  |> List.hd
  |> ParserContext.get_field ctx
  |> Option.get

let is_declared_field input str =
  if String.ends_with str ~suffix:"_is_nil" then false
  else
    let field = decode_field_aux str in
    ParserContext.is_declared_field input field

let is_declared_field_nil input str =
  if String.ends_with str ~suffix:"_is_nil" then
    let field = decode_field_aux str in
    ParserContext.is_declared_field input field
  else false

let is_declared_field_any input str =
  if String.ends_with str ~suffix:"_is_any" then
    let field = decode_field_aux str in
    ParserContext.is_declared_field input field
  else false
