open Commons.Try_log
open Builtin
open Parser.Lex_token
open Parser.Dusk_type
open Parser.Dusk_ast
open Resolve.Res_cont
open Codegen.Fin_type
open Codegen.Fin_ast
open Tc_cont

	(* error types *)

type tc_err =
	BadType_Err of g_type * g_type * l_pos
		(* functions *)
	| NoOverload_Err of string * g_type option * l_pos
		(* returns *)
	| EarlyReturn_Err of string * l_pos
	| NoReturn_Err of string * l_pos

type 'a tc_res = ('a, tc_err) try_res

let string_of_first_arg (t: g_type option): string = match t with
	None -> "empty arg list"
	| Some tau -> "type \"" ^ (string_of_type tau) ^ "\""

let string_of_tc_err (e: tc_err) = match e with
	| BadType_Err(s, t, p) -> "Expected type \"" ^ (string_of_type t) ^ "\" and received type \"" ^
		(string_of_type s) ^ "\" at " ^ (string_of_pos p) ^ "."
	| NoOverload_Err(f, t, p) -> "Function \"" ^ f ^ "\" does not have overload for " ^
		(string_of_first_arg t) ^ " at " ^ (string_of_pos p) ^ "."
	| EarlyReturn_Err(f, p) ->
		"Early return from function \"" ^ f ^ "\" producing unreachable code at " ^ (string_of_pos p) ^ "."
	| NoReturn_Err(f, p) ->
		"Declaration of function \"" ^ f ^ "\" does not return on all paths at " ^ (string_of_pos p) ^ "."
	
	(* expression type-checking *)

let tc_const (c: const): g_type = match c with
	IConst _ -> intTy
	| FConst _ -> floatTy
	| SConst _ -> stringTy
	| BConst _ -> boolTy

let rec tc_exp (env: type_env) (e: r_exp): (gen_exp * g_type) tc_res = match e with
	ConstExp(c, _) -> Valid (ConstExpC c, tc_const c)
	| VarExp(_, x, _) -> (match StringMap.find_opt x env.local with
		Some tau -> Valid (VarExpC x, tau)
		| _ -> failwith "BUG: tc_exp.ml - Failed variable lookup during type-checking phase."
	)
	| OpExp(_, _) -> failwith "BUG: tc_exp.ml - Operator expression in non-application position."
	(*| TupleExp(ctor, el, _) ->*)
	| AppExp(ef, el, _) ->
		let* et_l' = tc_exp_list env el in
		let tau_al = List.map snd et_l' in
		let el' = List.map fst et_l' in
		let* (f, (d, (_, tau_r))) = tc_fun_exp env ef (hd_opt tau_al) in (match d with
			BinaryASMSym _ -> failwith "UNIMPLEMENTED: tc_exp.ml - binary asm."
			| _ -> Valid (CallExpC(VarExpC f, el', tau_r), tau_r)
		)
	| _ -> failwith "UNIMPLEMENTED: tc_exp.ml - type-checking exp case."
and tc_fun_exp (env: type_env) (ef: r_exp) (tau_a: g_type option): (string * t_sym) tc_res = match ef with
	VarExp(_, f, p) -> (match lookup_fun_tenv env f tau_a with
		Some res -> Valid res
		| None -> Error (NoOverload_Err(f, tau_a, p))
	)
	| _ -> failwith "UNIMPLEMENTED: tc_exp.ml - function non-var case."
and tc_exp_list (env: type_env) (el: r_exp list): ((gen_exp * g_type) list) tc_res = match el with
	[] -> Valid []
	| e :: et ->
		let* res = tc_exp env e in
		let* res_t = tc_exp_list env et in Valid (res :: res_t)

	(* statement type-checking *)

let rec tc_stmt (_: string) (env: type_env) (s: r_stmt): (gen_stmt list * bool) tc_res = match s with
	EvalStmt(e, _) ->
		let* (e', _) = tc_exp env e in Valid ([EvalStmtC e'], false)
	| AssignStmt(x, e, _) -> (match StringMap.find_opt x env.local with
		Some _ -> let* (e', _) = tc_exp env e in Valid ([AssignStmtC(x, e')], false)
		| _ -> failwith "BUG: tc_exp.ml - Failed variable lookup during type-checking phase."
	)
	| _ -> failwith "UNIMPLEMENTED: tc_exp.ml - type-checking statement case."
and tc_body (f: string) (env: type_env) (b: r_stmt list): (gen_stmt list * bool) tc_res = match b with
	[] -> Valid ([], false)
	| s :: st ->
		let* (s', term0) = tc_stmt f env s in
		if term0 then Error (EarlyReturn_Err (f, ann_stmt s))
		else let* (st', termX) = tc_body f env st in Valid (s' @ st', termX)

	(* declaration / sectional type-checking *)

let rec add_param_list (env: type_env) (pl: (string * g_type) list): type_env = match pl with
	[] -> env
	| (x, tau) :: pt ->
		add_param_list { env with local = StringMap.add x tau env.local } pt

let tc_dec (env: type_env) (d: r_dec): ((string * gen_dec) list) tc_res = match d with
	FunDec(Method(f, pl, tau_r, b), p) ->
		let env' = add_param_list env pl in
		let* (b', term) = tc_body f env' b in
		if not term then (
			if tau_r <> unitTy then Error (NoReturn_Err(f, p))
			else Valid [(f, FunDecC (MethodC(pl, tau_r, b' @ [ReturnStmtC None])))]
		) else Valid [(f, FunDecC (MethodC(pl, tau_r, b')))]

let tc_section (env: type_env) (SectionR dl: r_section): ((string * gen_dec) list) tc_res =
	let rec tcs_rec dl = match dl with
		[] -> Valid []
		| (_, d) :: dt ->
			let* d' = tc_dec env d in
			let* dt' = tcs_rec dt in Valid (d' @ dt')
	in tcs_rec dl
