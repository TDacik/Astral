
module Logger = Logging.WithOutput (struct
  let name = "Synthesis"
  let level = 1
  let path = `Query "popper_query"
end)

let write temp_dir name what =
  let c = Out_channel.open_text (temp_dir ^ "/" ^ name) in
  Out_channel.output_string c what;
  Out_channel.close c

let synthesize ~bias ~knowledge ~examples () =
  let temp_dir = Filename.temp_dir "popper_query" "" in
  Logger.debug "Working directory: %s\n" temp_dir;
  Logger.output "bias.pl" Fun.id bias;
  Logger.output "bk.pl" Fun.id knowledge;
  Logger.output "exs.pl" Fun.id examples;

  write temp_dir "bias.pl" bias;
  write temp_dir "bk.pl" knowledge;
  write temp_dir "exs.pl" examples;
  let answer_filename, answer_channel = Filename.open_temp_file ~temp_dir "result" ".out" in
  let output = Unix.descr_of_out_channel answer_channel in
  let popper_path = FileUtils.(Config.PopperPath.get () / "popper-ilp") in
  let res =
    Unix.create_process
      popper_path
      [|popper_path; temp_dir|]
      Unix.stdin
      output
      output
  in
  close_out answer_channel;
  let _, status = Unix.waitpid [] res in
  let response = In_channel.with_open_text answer_filename In_channel.input_all in
  match status with
  | WEXITED 0 -> (true, response)
  | _ -> (false, response)
