open Commons.Try_log
open Parser
open Parser.Parse_commons
open Parser.Parse_exp
open Resolve
open Resolve.Res_type
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

let pre_compile_file (resEnv: Res_cont.res_env) (typeEnv: Tc_cont.type_env)
	(main_dir: string) (f: string): ((string * gen_dec) list) try_log_res =
		(* PHASE 1. lexing / parsing *)
	let*! tkList = load_token_list (main_dir ^ "/" ^ f) in
	let*! rawAst = tryWithErrLog string_of_parse_err (parseMain tkList) in
		(* PHASE 2. namespace resolution *)
	let resEnv' = Res_cont.freeze_env resEnv [] in
	(* tryWithErrLog string_of_rs_err (resolve_section resEnv rawAst) *)
	let*! canonAst = tryWithErrLog string_of_rs_err (resolve_section resEnv' rawAst) in
		(* PHASE 3. type-checking *)
	tryWithErrLog string_of_tc_err (tc_section typeEnv canonAst)

	(* command line argument state *)

let usage_msg = "dusk <directory>"

let main_arg = ref ""
let out_arg = ref ""
let win_arg = ref false
let target_arg = ref ""
let runtime_arg = ref ""

	(* system commands [TO REPLACE] *)

let fail_simple str = (print_string (str ^ "\n"); exit 1)

let run_command s e =
	let r = Sys.command s in if r <> 0 then fail_simple e else ()

	(* main program *)

let program _ =
		(* parse arguments from command line *)
	let binDir = Array.get Sys.argv 0 in
	Arg.parse [
		("-o", Arg.Set_string out_arg,
			"Name of the output executable.");
		("-w", Arg.Set win_arg,
			"Sets compilation to compile with mingw and target Windows.");
		("-t", Arg.Set_string target_arg,
			"Specifies a target architecture for the executable (other than the default).");
		("-r", Arg.Set_string runtime_arg,
			"The directory in which the compiler's runtime is stored (if not the default).")
	] (fun name -> main_arg := name) usage_msg;
		(* main compilation *)
	if !main_arg = "" then failLog "No file / directory name given."
	else let main_dir = !main_arg in (
		print_string ("Compiling: " ^ main_dir ^ "\n");
			(* builtin environments *)
		let resEnv = Res_cont.builtin_env () in
		let resBuiltins = resolve_builtins resEnv in
		let typeEnv = Tc_cont.builtin_tenv resBuiltins in
		let tcBuiltins = Tc_cont.tc_complete_builtins typeEnv resBuiltins in
			(* PHASES 1-3. incremental resolution *)
		let*! typedAst = pre_compile_file resEnv typeEnv main_dir "main.dm" in
			(* PHASE 4. code generation *)
		let targetArch =
			if !target_arg <> "" then Some !target_arg
			else if !win_arg then Some "x86_64-pc-windows-gnu" else None in
		genProgramHook targetArch (main_dir ^ "/test") false tcBuiltins typedAst;
			(* PHASE 5. linking *)
		let runDir = if !runtime_arg = "" then binDir ^ "/_runtime" else !runtime_arg in
			(* -- copy test object file to runtime directory *)
		run_command (Printf.sprintf "cp -f %s/test.o %s/j_out.o" main_dir runDir)
			("Failed to copy test binary to runtime directory\n  " ^ runDir ^ "");
			(* -- run makefile in runtime directory, copy output to output directory *)
		let runTarget = if !win_arg then "main.exe" else "main" in
		run_command (Printf.sprintf "cd %s; make %s" runDir runTarget) "Failed MAKE for runtime.";
		let outFile = if !out_arg = "" then runTarget else !out_arg in
		run_command (Printf.sprintf "cp %s/%s %s" runDir runTarget outFile)
			"Failed to copy final binary to output directory.";
		endLog ()
	);;

printLogRes (program ())