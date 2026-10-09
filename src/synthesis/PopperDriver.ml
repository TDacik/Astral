module Logger = Debug.SubQueryDir (struct
  let dirname = "synthesis"
  let name = "Synthesis"
  let level = 1
end)

  let write temp_dir name what =
  let c = Out_channel.open_text (temp_dir ^ "/" ^ name) in
  Out_channel.output_string c what;
  Out_channel.close c

let synthesize ~bias ~knowledge ~examples () =
  let temp_dir = Filename.temp_dir "popper_query" "" in
  ReportUtils.warning "Working directory: %s\n" temp_dir;
  write temp_dir "bias.pl" bias;
  write temp_dir "bk.pl" knowledge;
  write temp_dir "exs.pl" examples;
  let answer_filename, answer_channel = Filename.open_temp_file ~temp_dir "result" ".out" in
  let output = Unix.descr_of_out_channel answer_channel in
  let res =
    Unix.create_process
      "popper-ilp"
      [|"popper-ilp"; temp_dir|]
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
