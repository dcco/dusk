open Commons.Try_log
open Parser.Lex_token
open Parser.Dusk_type
open Resolve.Res_cont
open Codegen.Fin_ast

	(* error types *)

type tc_err =
	BadType_Err of m_type * m_type * l_pos

type 'a tc_res = ('a, tc_err) try_res

let string_of_tc_err (e: tc_err) = match e with
	| BadType_Err(s, t, p) -> "Expected type \"" ^ (string_of_type t) ^ "\" and received type \"" ^
		(string_of_type s) ^ "\" at " ^ (string_of_pos p) ^ "."

	(* expression type-checking *)

let tc_exp (e: r_exp): gen_exp tc_res = match e with
	ConstExp(c, _) -> Valid (ConstExpC c)
	| VarExp(_, x, _) -> Valid (VarExpC x)
	| OpExp(_, _) -> failwith "BUG: tc_exp.ml - Operator expression in non-application position."
	(*| TupleExp(ctor, el, _) ->*)
	| AppExp(ef, el, _) ->
		tc_fun_exp ef
	| _ -> failwith "UNIMPLEMENTED: tc_exp.ml - resolve exp case."
and tc_fun_exp (ef: r_exp): unit tc_res = match ef with
	VarExp(_, f, _) ->
		CallExpC(ef, )