open Commons.Try_log
open Commons.Tree_map
open Parser
open Parser.Dusk_ast
open Parser.Parse_commons
open Parser.Parse_exp
open Resolve
open Resolve.Res_type
open Resolve.Res_exp
open Tc
open Tc.Tc_err
open Tc.Tc_exp
open Codegen.Fin_ast
open Codegen.Gen_exp
open Rom.Rom_load

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

	(*
		file context: contextual information about what file is being compiled
			- toplevel
			- prelude: (module path, directory, file name)
			- library: (directory, file name)
	*)

type file_context =
	TopLevelFC
	| PreludeFC of string list * string * string
	| LibFC of string * string

let full_name_fc (main_dir: string) (fc: file_context): string = match fc with
	TopLevelFC -> main_dir ^ "/main.dm"
	| PreludeFC(_, dir, f) -> main_dir ^ "/" ^ dir ^ "/" ^ f
	| LibFC(dir, f) -> main_dir ^ "/" ^ dir ^ "/" ^ f

let full_dir_fc (main_dir: string) (fc: file_context): string = match fc with
	TopLevelFC -> main_dir
	| PreludeFC(_, dir, _) -> main_dir ^ "/" ^ dir
	| LibFC(dir, _) -> main_dir ^ "/" ^ dir

let prelude_fc (fc: file_context): bool = match fc with
	PreludeFC _ -> true
	| _ -> false

	(*
		pre-compilation:
			does compilation up to codegen (incrementally).
	*)

type pre_comp_res = (string * gen_dec) list

let print_lib_alert (l: string) (parent: string option): unit = match parent with
	None -> print_string ("Loading library: " ^ l ^ "\n")
	| Some p -> print_string ("Loading library: " ^ l ^ " (Requested by " ^ p ^ ")\n")

let rec pre_compile_lib (resEnv: Res_cont.res_env) (typeEnv: Tc_cont.type_env)
	(main_dir: string) (parent: string option) (libName: string): (pre_comp_res list) try_log_res =
		(* - lex / parse the library TOC *)
	print_lib_alert libName parent;
	let*! tkList = load_token_list (main_dir ^ "/" ^ libName ^ "/" ^ "toc.dt") in
	let*! Toc ml = tryWithErrLog string_of_parse_err (parseToc tkList) in
		(* - load un-compiled libraries *)
	let*! res_pre_ll = mapLogRes (fun (Module(_, rl, _, _)) ->
		let missingLibList = pre_check_req_list resEnv rl in
		let*! res_pre_ll = mapLogRes (pre_compile_lib resEnv typeEnv main_dir parent) missingLibList
		in validLog (List.concat res_pre_ll)
	) ml in
		(* - compile modules *)
	print_string ("Compiling library: " ^ libName ^ "\n");
	let*! res_l = mapLogRes (fun (Module(m, rl, xl, _)) ->
		print_string ("- Module: " ^ m ^ "\n");
			(* - resolve requirements (freeze imports) *)
		let resEnv' = Res_cont.freeze_env resEnv [libName; m] in
		let*! _ = tryWithErrLog string_of_rs_err (resolve_req_list resEnv' rl) in
			(* - compile individual files *)	
		let*! res_m = mapLogRes (fun x ->
			print_string ("-- " ^ x ^ "\n");
			let lfc = LibFC (libName ^ "/" ^ m, x ^ ".dm") in
			let typeEnv' = Tc_cont.with_dir_tenv typeEnv (libName ^ "/" ^ m) in
			let*! res_f = pre_compile_file resEnv' typeEnv' main_dir lfc
			in validLog (List.concat res_f)
		) xl in
		Res_cont.save_local_dec_env resEnv' [libName; m]; validLog res_m
	) ml in validLog ((List.concat res_pre_ll) @ (List.concat res_l))
		(* codegen for an individual file *)
and pre_compile_file (resEnv: Res_cont.res_env) (typeEnv: Tc_cont.type_env)
	(main_dir: string) (fc: file_context): (pre_comp_res list) try_log_res =
		(* PHASE 1. lexing / parsing *)
	(match fc with
		TopLevelFC -> print_string "Loading top-level\n"
		| PreludeFC(_, dir, f) -> print_string ("Loading prelude file \"" ^ dir ^ "/" ^ f ^ "\"\n")
		| _ -> ()
	);
	let*! tkList = load_token_list (full_name_fc main_dir fc) in
	let*! rawAst = tryWithErrLog string_of_parse_err (parseMain tkList) in
		(* PHASE 2. namespace resolution
			- for top-level, we must pre-load all the requirements
			- for not top-level, we have to save all the bindings
		*)
	let*! (res_pre_l, canonAst) = (match fc with
		TopLevelFC ->
				(* - load un-compiled libraries *)
			let Section(rl, _) = rawAst in
			let missingLibList = pre_check_req_list resEnv rl in
			let*! res_pre_ll = mapLogRes (pre_compile_lib resEnv typeEnv main_dir None) missingLibList in
				(* - resolve requirements (freeze imports) *)
			let resEnv' = Res_cont.freeze_env resEnv [] in
			let*! _ = tryWithErrLog string_of_rs_err (resolve_req_list resEnv' rl) in
				(* - file resolution *)
			print_string "Compiling top-level\n";
			let*! canonAst = tryWithErrLog string_of_rs_err (resolve_section resEnv' true rawAst) in
			validLog (List.concat res_pre_ll, canonAst)
		| _ ->
			let*! canonAst = tryWithErrLog string_of_rs_err (resolve_section resEnv (prelude_fc fc) rawAst) in
			validLog ([], canonAst)
	) in
		(* PHASE 3. type-checking *)
	let typeEnv' = Tc_cont.with_dir_tenv typeEnv (full_dir_fc main_dir fc) in
	let*! res_f = tryWithErrLog string_of_tc_err (tc_section typeEnv' canonAst) in
	validLog (res_pre_l @ [res_f])
		(*
			codegen for an individual file + adds its bindings to an existing library
				(used for pipeline/shader code)
		*)
and ext_compile_file (resEnv: Res_cont.res_env) (typeEnv: Tc_cont.type_env)
	(main_dir: string) (path: string list) (dir: string) (f: string): (pre_comp_res list) try_log_res =
	let resEnv' = Res_cont.freeze_env resEnv path in
	let*! res_f = pre_compile_file resEnv' typeEnv main_dir (PreludeFC(path, dir, f)) in
	Res_cont.save_ext_dec_env resEnv' path; validLog res_f

(*
let pre_compile_file (resEnv: Res_cont.res_env) (typeEnv: Tc_cont.type_env)
	(main_dir: string) (f: string): ((string * gen_dec) list) try_log_res =
		(* PHASE 1. lexing / parsing *)
	let*! tkList = load_token_list (main_dir ^ "/" ^ f) in
	let*! rawAst = tryWithErrLog string_of_parse_err (parseMain tkList) in
		(* PHASE 2. namespace resolution *)
	let resEnv' = Res_cont.freeze_env resEnv [] in
	let*! canonAst = tryWithErrLog string_of_rs_err (resolve_section resEnv' rawAst) in
		(* PHASE 3. type-checking *)
	tryWithErrLog string_of_tc_err (tc_section typeEnv canonAst)*)

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
		print_string ("Source directory: " ^ main_dir ^ "\n");
			(* build virtual bindings from builtin + rom *)
		let romBindings = read_rom_layout (main_dir ^ "/rom") in
		let virtTree = add_tree (Builtin.builtinTreeMap ()) ["Sys"; "Rom"] romBindings in
		let virtBindings = List.concat (List.map (fun (path, vdl) ->
			List.map (fun vd -> (path, vd)) vdl
		) (flatten_tree virtTree)) in
			(* build environments from virtual bindings *)
		let resEnv = Res_cont.builtin_env virtTree in
		let canonBindings = resolve_virt_bindings resEnv virtBindings in
		let typeEnv = Tc_cont.builtin_tenv canonBindings in
		let tcBuiltins = Tc_cont.tc_complete_builtins typeEnv canonBindings in
			(* compile shader/pipeline *)
		let*! pipeAstList = ext_compile_file resEnv typeEnv main_dir ["Sys"; "Sulfur"] "shader" "pipeline.dm" in
			(* PHASES 1-3. incremental resolution *)
		let*! typedAstList = pre_compile_file resEnv typeEnv main_dir TopLevelFC in
		let typedAst = List.concat (pipeAstList @ typedAstList) in
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