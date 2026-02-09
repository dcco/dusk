open Commons.Try_log
open Parser.Lex_token
open Parser.Dusk_ast
open Res_cont

	(* error types *)

type resolve_err =
	BadLookup_Err of string option * string * l_pos
	| AmbiguousLookup_Err of string option * string * l_pos

type 'a rs_res = ('a, resolve_err) try_res

let string_of_name (prefix: string option) (x: string): string = match prefix with
	None -> x
	| Some m -> m ^ "." ^ x

let string_of_rs_err (e: resolve_err) = match e with
	BadLookup_Err(prefix, x, p) -> "Bad lookup of \"" ^ (string_of_name prefix x) ^ "\" at " ^ (string_of_pos p) ^ "."
	| AmbiguousLookup_Err(prefix, x, p) -> "Ambiguous lookup of \"" ^ (string_of_name prefix x) ^ "\" at " ^ (string_of_pos p) ^ "."

	(* expression resolution *)

let rec resolve_exp (env: res_env) (e: n_exp): r_exp rs_res = match e with
	ConstExp(c, p) -> Valid (ConstExp(c, p))
	| VarExp(prefix, x, p) -> (match lookup_env env prefix x with
		[(_, x')] -> Valid (VarExp((), x', p))
		| [] -> Error (BadLookup_Err(prefix, x, p))
		| _ -> Error (AmbiguousLookup_Err(prefix, x, p))
	)
	| TupleExp(ctor, el, p) ->
		let* ctor' = (match ctor with
			None -> Valid None
			| Some (prefix, x) -> (match lookup_env env prefix x with
				[(_, x')] -> Valid (Some ((), x'))
				| [] -> Error (BadLookup_Err(prefix, x, p))
				| _ -> Error (AmbiguousLookup_Err(prefix, x, p))
			)
		) in let* el' = resolve_exp_list env el in Valid (TupleExp(ctor', el', p))
	| AppExp(ef, el, p) ->
		let* ef' = resolve_exp env ef in
		let* el' = resolve_exp_list env el in Valid (AppExp(ef', el', p))
	| _ -> failwith "UNIMPLEMENTED: res_exp.ml - resolve exp case."
and resolve_exp_list (env: res_env) (el: n_exp list): (r_exp list) rs_res = match el with
	[] -> Valid []
	| e :: et ->
		let* e' = resolve_exp env e in
		let* et' = resolve_exp_list env et in Valid (e' :: et')

	(* statement resolution *)

let rec resolve_stmt (env: res_env) (s: n_stmt): r_stmt rs_res = match s with
	EvalStmt(e, p) -> let* e' = resolve_exp env e in Valid (EvalStmt(e', p))
	| AssignStmt(x, e, p) -> let* e' = resolve_exp env e in Valid (AssignStmt(x, e', p))
	| ReturnStmt(e, p) -> (match e with
		None -> Valid (ReturnStmt(None, p))
		| Some e ->
			let* e' = resolve_exp env e in Valid (ReturnStmt(Some e', p))
	)
	| IfStmt(e, b1, b2, p) ->
		let* e' = resolve_exp env e in
		let* b1' = resolve_body env b1 in
		let* b2' = resolve_body env b2 in Valid (IfStmt(e', b1', b2', p))
	| WhileStmt(e, b, p) ->
		let* e' = resolve_exp env e in
		let* b' = resolve_body env b in Valid (WhileStmt(e', b', p))
	| _ -> failwith "UNIMPLEMENTED: res_exp.ml - resolve stmt case."
and resolve_body (env: res_env) (b: n_stmt list): (r_stmt list) rs_res = match b with
	[] -> Valid []
	| s :: st ->
		let* s' = resolve_stmt env s in
		let* st' = resolve_body env st in Valid (s' :: st')

	(* declaration resolution *)

let resolve_dec (env: res_env) (d: n_dec): (string * r_dec) rs_res = match d with
	FunDec(Method(f, param_l, tau_r, b), _) ->
		let* b' = resolve_body env b in Valid (f, FunDecR(Method(f, param_l, tau_r, b')))

let rec resolve_dec_list (env: res_env) (dl: n_dec list): ((string * r_dec) list) rs_res = match dl with
	[] -> Valid []
	| d :: dt ->
		let* (f, d') = resolve_dec env d in
		let* dt' = resolve_dec_list env dt in Valid ((f, d') :: dt')

	(* section resolution *)

let resolve_section (env: res_env) (s: n_section): r_section rs_res = match s with
	Section(_, dl) ->
		let* dl' = resolve_dec_list env dl in Valid (SectionR dl')
