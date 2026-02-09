open Commons.Try_log
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

type 'a tc_res = ('a, tc_err) try_res

let string_of_tc_err (e: tc_err) = match e with
	| BadType_Err(s, t, p) -> "Expected type \"" ^ (string_of_type t) ^ "\" and received type \"" ^
		(string_of_type s) ^ "\" at " ^ (string_of_pos p) ^ "."

	(* auxiliary functions *)

let hd_opt (l: 'a list): 'a option = match l with
	[] -> None
	| v :: _ -> Some v

	(* expression type-checking *)

let tc_const (c: const): g_type = match c with
	IConst _ -> intTy
	| FConst _ -> floatTy
	| SConst _ -> stringTy
	| BConst _ -> boolTy

let rec tc_exp (env: type_env) (e: r_exp): (gen_exp * g_type) tc_res = match e with
	ConstExp(c, _) -> Valid (ConstExpC c, tc_const c)
	| VarExp(_, x, _) -> (match Hashtbl.find_opt env x with
		Some 
		Valid (VarExpC x, intTy)
	)
	| OpExp(_, _) -> failwith "BUG: tc_exp.ml - Operator expression in non-application position."
	(*| TupleExp(ctor, el, _) ->*)
	(*| AppExp(ef, el, _) ->
		let* tau_al = tc_exp_list el in
		let* (tau_pl, tau_r) = tc_fun_exp ef (hd_opt tau_al) in
		(CallExpC, tau_r)*)
	| _ -> failwith "UNIMPLEMENTED: tc_exp.ml - resolve exp case."
(*and tc_fun_exp (ef: r_exp) (tau_a: ): unit tc_res = match ef with
	VarExp(_, f, _) ->
		CallExpC(ef, )*)
and tc_exp_list (env: type_env) (el: r_exp list): ((gen_exp * g_type) list) tc_res = match el with
	[] -> Valid []
	| e :: et ->
		let* res = tc_exp env e in
		let* res_t = tc_exp_list env et in Valid (res :: res_t)


	(*
		array/map operations are fundamentally different because they are intrinsically polymorphic.
			we want their operators to be overloadable though, so we give them special types.
	*)
