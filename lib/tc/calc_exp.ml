open Parser.Lex_token
open Commons.Try_log
open Codegen.Fin_ast
open Tc_err
open Tc_cont

	(*
		compile-time constant expression calculation
	*)

let as_int (e: gen_exp): int = match e with
	ConstExpC (IConst i) -> i
	| _ -> failwith "BUG: calc_exp.ml - Found unexpected non-integer value during constant calculation."

let rec calc_exp (env: type_env) (e: gen_exp): gen_exp tc_res = match e with
	ConstExpC c -> Valid (ConstExpC c)
	| NullExpC -> Valid NullExpC
	| NewArrayExpC(dim_l, el, tau) ->
		let* dim_l' = map_try_res (calc_exp env) dim_l in
		let dims = List.map as_int dim_l' in
		let* el' = map_try_res (calc_exp env) el in
		Valid (ConstArrayExpC(dims, el', tau))
	| _ -> failwith "UNIMPLEMENTED: calc_exp.ml - Constant expression."

let calc_cfun (env: type_env) (f: string) (el: gen_exp list) (p: l_pos): gen_exp tc_res = match (f, el) with
	("cLoad", [ConstExpC (SConst s)]) -> (match env.curDir with
		Some dir -> 
			let fname = dir ^ "/" ^ s in
			if not (Sys.file_exists fname) then Error (BadCLoad_Err(fname, p))
			else (try
				let fc = open_in fname in
				let len = in_channel_length fc in
				let text = really_input_string fc len in
				close_in fc; Valid (ConstExpC (SConst text))
				with _ -> Error (FailCLoad_Err p)
			)
		| _ -> failwith "BUG: calc_exp.ml - Constant expression cLoad with unspecified directory."
	)
	| ("null", _) -> Valid NullExpC
	| _ -> failwith "UNIMPLEMENTED: calc_exp.ml - Constant expression cfun failure."