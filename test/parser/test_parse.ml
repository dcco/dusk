open Commons.Try_log
open Parser.Parse_hook

	(*
		basic test framework
	*)

let case_files case_dir =
	Sys.readdir case_dir
	|> Array.to_list
	|> List.map (Filename.concat case_dir)

let parse_hook path () =
	hookLogRes (fun e ->
		Alcotest.failf "Parse failed for %s:\n%s" path e
	) (parseFile path)

let parser_tests () =
  case_files "cases"
  |> List.map (fun path ->
       let name = Filename.basename path in
       Alcotest.test_case name `Quick (parse_hook path))

let () =
	Alcotest.run "Parser" [
		("main tests", parser_tests ())
	];;
