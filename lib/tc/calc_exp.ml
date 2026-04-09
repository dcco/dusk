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
	| NewArrayExpC(dim_l, el, tau) ->
		let* dim_l' = map_try_res (calc_exp env) dim_l in
		let dims = List.map as_int dim_l' in
		let* el' = map_try_res (calc_exp env) el in
		Valid (ConstArrayExpC(dims, el', tau))
	| _ -> failwith "UNIMPLEMENTED: calc_exp.ml - Constant expression."