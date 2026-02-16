open Commons.Try_log
open Parser
open Parser.Parse_commons
open Parser.Parse_exp
open Resolve
open Resolve.Res_exp
open Tc
open Tc.Tc_exp
open Codegen.Fin_ast
open Codegen.Gen_exp

	(*
		preproc phase (incremental steps):
		- handles the compilation steps that happen incrementally (parsing, name resolution)
	*)

let load_token_list (fname: string): (Lex_token.token list) try_log_res =
	if not (Sys.file_exists fname) then failLog ("Could not find file " ^ fname)
	else let fc = open_in fname in
		let lexbuf = Lexing.from_channel fc in
		let rec lex_iter partList = 
			let tk = Man_lex.token lexbuf in
				if fst tk = Lex_token.EOF then List.rev partList
				else lex_iter (tk :: partList) in
		let tkList = lex_iter [] in
		(close_in fc; validLog tkList);;

let pre_compile_file (typeEnv: Tc_cont.type_env) (main_dir: string) (f: string): ((string * gen_dec) list) try_log_res =
		(* PHASE 1. lexing / parsing *)
	let*! tkList = load_token_list (main_dir ^ "/" ^ f) in
	let*! rawAst = tryWithErrLog string_of_parse_err (parseMain tkList) in
		(* PHASE 2. namespace resolution *)
	let resEnv = Res_cont.builtin_env () in
	(* tryWithErrLog string_of_rs_err (resolve_section resEnv rawAst) *)
	let*! canonAst = tryWithErrLog string_of_rs_err (resolve_section resEnv rawAst) in
		(* PHASE 3. type-checking *)
	tryWithErrLog string_of_tc_err (tc_section typeEnv canonAst)

	(* command line argument state *)

let usage_msg = "dusk <directory>"

let main_arg = ref ""
let out_arg = ref ""
let win_arg = ref false
let target_arg = ref ""
let runtime_arg = ref ""

	(* main program *)

let program _ =
		(* parse arguments from command line *)
	Arg.parse [
		("-o", Arg.Set_string out_arg,
			"Name of the output executable.");
		("-w", Arg.Set win_arg,
			"Sets compilation to compile with mingw and target Windows.");
		("-t", Arg.Set_string target_arg,
			"Specifies a target architecture for the executable (other than the default).");
		"-r", Arg.Set_string runtime_arg,
			"The directory in which the compiler's runtime is stored (if not the default)."
	] (fun name -> main_arg := name) usage_msg;
		(* main compilation *)
	if !main_arg = "" then failLog "No file / directory name given."
	else let main_dir = !main_arg in (
		print_string ("Compiling: " ^ main_dir ^ "\n");
		(* builtin environments *)
		let typeEnv = Tc_cont.builtin_tenv () in
		(* PHASES 1-3. incremental resolution *)
		let*! typedAst = pre_compile_file typeEnv main_dir "main.dm" in
		(* PHASE 4. code generation *)
		let targetArch =
			if !target_arg <> "" then Some !target_arg
			else if !win_arg then Some "x86_64-pc-windows-gnu" else None in
		genProgramHook targetArch (main_dir ^ "/test") false (Tc_cont.sym_list_tenv typeEnv) typedAst ;
		endLog ()
	);;

printLogRes (program ())