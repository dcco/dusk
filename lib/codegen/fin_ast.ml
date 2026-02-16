open Parser.Dusk_ast
open Fin_type

	(*
		representation for raw code
	*)

type gen_exp =
	ConstExpC of const
	(*| LitExpC of int*)
	| VarExpC of string
	| BinExpC of string * gen_exp * gen_exp
	| CallExpC of gen_exp * gen_exp list * g_type

type gen_stmt =
	EvalStmtC of gen_exp
	| AssignStmtC of string * gen_exp
	| ReturnStmtC of gen_exp option
	| VarStmtC of string * gen_exp
	| IfStmtC of gen_exp * gen_stmt list * bool * gen_stmt list * bool
	| WhileStmtC of gen_exp * gen_stmt list

type gen_met =
	MethodC of (string * g_type) list * g_type * gen_stmt list

let type_of_method (MethodC(arg_l, tau_r, _): gen_met): g_type list * g_type =
	(List.map (fun (_, tau) -> tau) arg_l, tau_r)

type gen_dec =
	FunDecC of gen_met