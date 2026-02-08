open Commons.Try_log
open Man_lex
open Dusk_ast
open Parse_commons
open Parse_exp

let loadTokenList (fname: string): (Lex_token.token list) try_log_res =
	if not (Sys.file_exists fname) then failLog ("Could not find file " ^ fname)
	else let fc = open_in fname in
		let lexbuf = Lexing.from_channel fc in
		let rec lex_iter partList = 
			let tk = token lexbuf in
				if fst tk = Lex_token.EOF then List.rev partList
				else lex_iter (tk :: partList) in
		let tkList = lex_iter [] in
		(close_in fc; validLog tkList);;

let parseFile (fname: string): n_section try_log_res =
	let*! tkList = loadTokenList fname in
	tryWithErrLog string_of_parse_err (parseMain tkList)



