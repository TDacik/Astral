open MemoryModel

let field field =
  Format.asprintf "%sX%s" (Field.get_name field) (Sort.name @@ Field.get_sort field)

let decode_field str =
  BatString.split_on_char 'X' str
  |> List.hd

let is_declared_field input str =
  if String.ends_with str ~suffix:"_is_nil" then false
  else
    let field = decode_field str in
    Format.printf "%s -> %s\n" str field;
    ParserContext.is_declared_field input field

let is_declared_field_nil input str =
  if String.ends_with str ~suffix:"_is_nil" then
    let field = decode_field str in
    Format.printf "%s -> %s\n" str field;
    ParserContext.is_declared_field input field
  else false
