open Commons.Try_log
open Parser.Lex_token
open Parser.Dusk_type
open Parser.Dusk_ast
open Res_cont
open Res_type

	(* basic name resolution *)

let resolve_name (env: res_env) (p: l_pos) (prefix: qual_tag) (x: string): string rs_res = match lookup_env env prefix x with
	[(ox, x')] -> Valid (canonize_binding env ox x')
	| [] -> Error (BadLookup_Err(prefix, x, p))
	| _ -> Error (AmbiguousLookup_Err(prefix, x, p))

	(* expression resolution *)

let rec resolve_exp (env: res_env) (e: n_exp): r_exp rs_res = match e with
	ConstExp(c, p) -> Valid (ConstExp(c, p))
	| OpExp(xop, p) -> Valid (OpExp(xop, p))
	| VarExp(prefix, x, p) -> let* x' = resolve_name env p prefix x in Valid (VarExp(CT, x', p))
	| TupleExp(ctor, el, p) ->
		let* ctor' = opt_try_res (fun (prefix, x) ->
			let* x' = resolve_name env p prefix x in Valid (CT, x')
		) ctor in
		let* el' = resolve_exp_list env el in Valid (TupleExp(ctor', el', p))
	| NewStructExp(prefix, x, fl, p) ->
		let* x' = resolve_name env p prefix x in
		let* fl' = map_try_res (fun (f, e) ->
			let* e' = resolve_exp env e in Valid (f, e')
		) fl in Valid (NewStructExp(CT, x', fl', p))
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

let rec resolve_stmt (env: res_env) (s: n_stmt): (res_env * r_stmt) rs_res = match s with
	EvalStmt(e, p) -> let* e' = resolve_exp env e in Valid (env, EvalStmt(e', p))
	| AssignStmt(x, e, p) -> let* e' = resolve_exp env e in Valid (env, AssignStmt(x, e', p))
	| ReturnStmt(e, p) -> (match e with
		None -> Valid (env, ReturnStmt(None, p))
		| Some e ->
			let* e' = resolve_exp env e in Valid (env, ReturnStmt(Some e', p))
	)
	| PatStmt(px, e, p) ->
		let* e' = resolve_exp env e in
		let env' = (match px with
			VarPat x -> { env with localIds = StringMap.add x () env.localIds }
			| ListPat xol -> List.fold_left (fun env' xo -> match xo with
				None -> env' | Some x -> { env' with localIds = StringMap.add x () env'.localIds }
			) env xol
		) in Valid (env', PatStmt(px, e', p))
	| IfStmt(e, b1, b2, p) ->
		let* e' = resolve_exp env e in
		let* (_, b1') = resolve_body env b1 in
		let* (_, b2') = resolve_body env b2 in Valid (env, IfStmt(e', b1', b2', p))
	| WhileStmt(e, b, p) ->
		let* e' = resolve_exp env e in
		let* (_, b') = resolve_body env b in Valid (env, WhileStmt(e', b', p))
	| ForStmt(x, rt, e, b, p) ->
		let* e' = resolve_exp env e in
		let env' = { env with localIds = StringMap.add x () env.localIds } in
		let* (_, b') = resolve_body env' b in Valid (env, ForStmt(x, rt, e', b', p))
and resolve_body (env: res_env) (b: n_stmt list): (res_env * r_stmt list) rs_res = match b with
	[] -> Valid (env, [])
	| s :: st ->
		let* (env2, s') = resolve_stmt env s in
		let* (env3, st') = resolve_body env2 st in Valid (env3, s' :: st')

	(* declaration resolution *)

let resolve_dec (env: res_env) (d: n_dec): r_dec rs_res = match d with
	FunDec(Method(f, param_l, tau_r, b), p) ->
		let fName = canonize_scope env.curPath f in
		let* param_l' = map_try_res (fun (x, tau_p) ->
			let* tau_p' = resolve_type env p tau_p in Valid (x, tau_p')
		) param_l in
		let* tau_r' = resolve_type env p tau_r in
		add_local_dec_env env f;
		let* (_, b') = resolve_body env b in Valid (FunDec(Method(fName, param_l', tau_r', b'), p))
	| TDefDec(x, td, p) ->
		let tName = canonize_scope env.curPath x in
		add_local_dec_env env x;
		let* td' = resolve_type_def env p td in	Valid (TDefDec(tName, td', p))

let rec resolve_dec_list (env: res_env) (dl: n_dec list): (r_dec list) rs_res = match dl with
	[] -> Valid []
	| d :: dt ->
		let* d' = resolve_dec env d in
		let* dt' = resolve_dec_list env dt in Valid (d' :: dt')

	(* requirement resolution *)

let resolve_req (env: res_env) (r: n_req): unit = match r with
	ShortRefReq(path, _) ->
		let handle = List.nth path ((List.length path) - 1) in
		add_import_env env path handle
	| LongRefReq(path, ml, _) ->
		List.iter (fun handle -> add_import_env env (path @ [handle]) handle) ml

let rec resolve_req_list (env: res_env) (rl: n_req list): unit = match rl with
	[] -> ()
	| r :: rt -> resolve_req env r; resolve_req_list env rt

	(*
		section resolution
		- modifies the environment, but only the global section
	*)

let resolve_section (env: res_env) (s: n_section): r_section rs_res = match s with
	Section(rl, dl) ->
		resolve_req_list env rl;
		let* dl' = resolve_dec_list env dl in Valid (SectionR dl')
