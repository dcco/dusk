open Commons.Try_log
open Commons.Tree_map
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
	| DataArrayExp(i, tau_o, dim_l, el, p) ->
		let* tau_o' = opt_try_res (resolve_type env p) tau_o in
		let* el' = resolve_exp_list env el in Valid (DataArrayExp(i, tau_o', dim_l, el', p))
	| FormatArrayExp(i, dim_l, e, p) ->
		let* dim_l' = resolve_exp_list env dim_l in
		let* e' = resolve_exp env e in Valid (FormatArrayExp(i, dim_l', e', p))
	| NewStructExp(prefix, x, fl, p) ->
		let* x' = resolve_name env p prefix x in
		let* fl' = map_try_res (fun (f, e) ->
			let* e' = resolve_exp env e in Valid (f, e')
		) fl in Valid (NewStructExp(CT, x', fl', p))
	| AppExp(ef, el, p) ->
		let* ef' = resolve_exp env ef in
		let* el' = resolve_exp_list env el in Valid (AppExp(ef', el', p))
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
	| GCCollectStmt p -> Valid (env, GCCollectStmt p)
and resolve_body (env: res_env) (b: n_stmt list): (res_env * r_stmt list) rs_res = match b with
	[] -> Valid (env, [])
	| s :: st ->
		let* (env2, s') = resolve_stmt env s in
		let* (env3, st') = resolve_body env2 st in Valid (env3, s' :: st')

	(* declaration resolution *)

let resolve_dec (env: res_env) (d: n_dec): r_dec rs_res = match d with
	FunDec(Method(lf, f, param_l, tau_r, b), p) ->
			(* resolve name as new function OR overload *)
		let bind_l = add_local_dec_env_ol env f in
		if List.length bind_l > 1 then Error (AmbiguousLookup_Err(QT None, f, p))
		else (
			let (ox, f') = List.hd bind_l in
			let fName = canonize_binding env ox f' in
				(* get parameter + return types *)
			let* param_l' = map_try_res (fun (x, tau_p) ->
				let* tau_p' = resolve_type env p tau_p in Valid (x, tau_p')
			) param_l in
			let* tau_r' = resolve_type env p tau_r in
				(* update environment, resolve function body *)
			let env' = List.fold_left (fun env' (x, _) ->
				{ env' with localIds = StringMap.add x () env'.localIds }
			) env param_l' in
			let* (_, b') = resolve_body env' b in Valid (FunDec(Method(lf, fName, param_l', tau_r', b'), p))
		)
	| TDefDec(x, td, p) ->
		let tName = add_local_dec_env env x in
		let* td' = resolve_type_def env p td in	Valid (TDefDec(tName, td', p))
	| ConstDec(x, e, p) ->
		let x' = add_local_dec_env env x in
		let* e' = resolve_exp env e in
		Valid (ConstDec(x', e', p))

let rec resolve_dec_list (env: res_env) (dl: n_dec list): (r_dec list) rs_res = match dl with
	[] -> Valid []
	| d :: dt ->
		let* d' = resolve_dec env d in
		let* dt' = resolve_dec_list env dt in Valid (d' :: dt')

	(* requirement resolution *)

let resolve_req (env: res_env) (r: n_req): unit rs_res = match r with
	ShortRefReq(path, p) ->
		let handle = List.nth path ((List.length path) - 1) in
		if not (valid_path_import_env env path) then Error (BadReq_Err(path, p))
		else (add_import_env env path handle; Valid ())
	| LongRefReq(path, ml, p) ->
		let rec rr_rec hl = (match hl with
			[] -> Valid ()
			| handle :: ht ->
				let path' = path @ [handle] in
				if not (valid_path_import_env env path') then Error (BadReq_Err(path', p))
				else (add_import_env env path' handle; rr_rec ht)
		) in rr_rec ml

let rec resolve_req_list (env: res_env) (rl: n_req list): unit rs_res = match rl with
	[] -> Valid ()
	| r :: rt -> let* _ = resolve_req env r in resolve_req_list env rt

	(*
		section resolution
		- modifies the environment, but only the global section
	*)

let resolve_section (env: res_env) (top: bool) (Section(rl, dl): n_section): r_section rs_res =
	let* _ = if not top then (match rl with
		[] -> Valid ()
		| r :: _ -> Error (NonEmptyReqList_Err (ann_req r))
	) else resolve_req_list env rl
	in let* dl' = resolve_dec_list env dl in Valid (SectionR dl')

	(*
		pre-check, returns list of libraries that need to be compiled
	*)

let pre_check_req_list (env: res_env) (rl: n_req list): string list =
	let ackMissingLib p =
		if has_branch_tree !(env.globalModules) p then []
		else (env.globalModules := stub_tree !(env.globalModules) [p]; [p])
	in List.concat (List.map (fun r -> match r with
		ShortRefReq(path, _) -> ackMissingLib (List.hd path)
		| LongRefReq(path, _, _) -> ackMissingLib (List.hd path)
	) rl)

(*
let pre_check_section (env: res_env) (Section(rl, _): n_section): string list =
	pre_check_req_list rl

let pre_check_toc (env: res_env) (Toc(rl, _): n_toc): string list =
	List.concat (List.map precheck_req rl) *)