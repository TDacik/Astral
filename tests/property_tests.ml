(*
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2025 *)

let roundtrip_test_sl () =
  let roundtrip phi =
    let tmp = Filename.temp_file "astral_test" ".smt2" in
    SL.output_benchmark tmp phi;
    let input = Parser.parse_file tmp in
    SL.print ~prefix:"Original:\n" phi;
    SL.print ~prefix:"After roundtrip:\n" (ParserContext.get_phi input);
    SL.(===) phi (ParserContext.get_phi input)
  in

  let test =
    QCheck2.Test.make ~count:100 ~print:SL.to_smtlib ~name:"roundtrip" ArbitrarySL.Default.entailment roundtrip
  in
  QCheck.Test.check_exn test

let () =
  run "Property tests" [
    "[SL] output and parse", [
      test_case "" `Quick roundtrip_test_sl;
    ]
  ]
