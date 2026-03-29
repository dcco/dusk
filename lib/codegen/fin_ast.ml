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
		(* boxes a value inside a pointer *)
	| BoxExpC of int * gen_exp * g_type
		(* tuple operations *)
	| TupleExpC of gen_exp list
	| TagTupleExpC of string * gen_exp list
	| TupleIndexExpC of gen_exp * int * g_type
		(* struct operations *)
	| NewStructExpC of gen_exp list

type gen_stmt =
	EvalStmtC of gen_exp
	| AssignStmtC of string * gen_exp
	| ReturnStmtC of gen_exp option
	| VarStmtC of string * gen_exp * g_type
	| IfStmtC of gen_exp * gen_stmt list * bool * gen_stmt list * bool
	| WhileStmtC of gen_exp * gen_stmt list

type gen_met =
	MethodC of (string * g_type) list * g_type * gen_stmt list

let type_of_method (MethodC(arg_l, tau_r, _): gen_met): g_type list * g_type =
	(List.map (fun (_, tau) -> tau) arg_l, tau_r)

type gen_dec =
	FunDecC of gen_met

	(*
		auxiliary gathering functions
	*)

let rec collect_box_exp (e: gen_exp): (int * gen_exp * g_type) list = match e with
	BoxExpC(i, e, tau) -> [(i, e, tau)]
	| BinExpC(_, e1, e2) -> (collect_box_exp e1) @ (collect_box_exp e2)
	| CallExpC(ef, el, _) -> (collect_box_exp ef) @ (List.concat (List.map collect_box_exp el))
	| TupleExpC el -> List.concat (List.map collect_box_exp el)
	| TagTupleExpC(_, el) -> List.concat (List.map collect_box_exp el)
	| TupleIndexExpC(e, _, _) -> collect_box_exp e
	| _ -> []

let rec collect_box_stmt (s: gen_stmt): (int * gen_exp * g_type) list = match s with
	EvalStmtC e -> collect_box_exp e
	| AssignStmtC(_, e) -> collect_box_exp e
	| ReturnStmtC eo -> (match eo with None -> [] | Some e -> collect_box_exp e)
	| VarStmtC(_, e, _) -> collect_box_exp e
	| IfStmtC(e, b1, _, b2, _) -> (collect_box_exp e) @ (collect_box_body b1) @ (collect_box_body b2)
	| WhileStmtC(e, b) -> (collect_box_exp e) @ (collect_box_body b)
and collect_box_body (b: gen_stmt list): (int * gen_exp * g_type) list = match b with
	[] -> []
	| s :: st -> (collect_box_stmt s) @ (collect_box_body st)

let rec collect_var_stmt (s: gen_stmt): (string * g_type) list = match s with
	EvalStmtC _ -> []
	| AssignStmtC(_, _) -> []
	| ReturnStmtC _ -> []
	| VarStmtC(x, _, tau) -> [(x, tau)]
	| IfStmtC(_, b1, _, b2, _) -> (collect_var_body b1) @ (collect_var_body b2)
	| WhileStmtC(_, b) -> collect_var_body b
and collect_var_body (b: gen_stmt list): (string * g_type) list = match b with
	[] -> []
	| s :: st -> (collect_var_stmt s) @ (collect_var_body st)