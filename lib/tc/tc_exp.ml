open Commons.Try_log
open Builtin
open Parser.Lex_token
open Parser.Dusk_type
open Parser.Dusk_ast
open Resolve.Res_cont
open Codegen.Fin_type
open Codegen.Fin_ast
open Tc_cont
open Tc_err
open Calc_exp
	
	(* type-checking basics *)

let is_heap_type (env: type_env) (tau: g_type): bool = match tau with
	NamedTy(_, cx) -> (match Hashtbl.find_opt env.globalTIds cx with
		Some (TcTDef td) -> (match td with
			StructTD _ -> true
			| _ -> false
		)
		| _ -> failwith "BUG: tc_exp.ml - Invalid type encountered while checking heap type status."
	)
	| ArrayTy(_, _) -> true
	| _ -> false

	(* type-checking auxiliaries *)

type g_fun =
	UnaryGF of string
	| BinaryGF of string
	| InternalGF of string
	| TupleIndexGF of int
	| ArrayIndexGF of rw
	| ArrayLengthGF
	| ArrayDimsGF of int
	| StructFieldGF of rw * int * string
	| CallGF of int list

let rec index_type_list (tau_l: g_type list) (i: int): g_type option = match tau_l with
	[] -> None
	| tau :: tau_t -> if i = 0 then Some tau else index_type_list tau_t (i - 1)

let lookup_field_list (fl: (string * g_type) list) (x: string): (g_type * int) option =
	let rec lfl_rec fl i = match fl with
		[] -> None
		| (y, tau) :: ft -> if x = y then Some (tau, i) else lfl_rec ft (i + 1)
	in lfl_rec fl 0

	(* basic type checks *)

let rec is_subtype (s: g_type) (t: g_type): bool = match (s, t) with
	(PrimTy s', PrimTy t') -> s' = t'
	| (BuiltinTy s', BuiltinTy t') -> s' = t'
	| (NamedTy(_, s'), NamedTy(_, t')) -> s' = t'
	| (TupleTy _sl, TupleTy _tl) ->
		if List.length _sl <> List.length _tl then false
		else List.for_all (fun (s, t) -> is_subtype s t) (List.combine _sl _tl)
	| (ArrayTy(i, s'), ArrayTy(j, t')) -> i = j && is_subtype s' t'
	| _ -> false

let tc_type (s: g_type) (t: g_type) (p: l_pos): unit tc_res =
	if is_subtype s t then Valid () else Error (BadType_Err(s, t, p))

let tc_type_list (_sl: g_type list) (_tl: g_type list) (p: l_pos): unit tc_res =
	if List.length _sl <> List.length _tl then Error (MismatchedArgNum_Err(List.length _sl, List.length _tl, p))
	else let rec tctl_rec _sl _tl = match (_sl, _tl) with
		([], []) -> Valid ()
		| (s :: st, t :: tt) ->
			let* _ = tc_type s t p in tctl_rec st tt
		| _ -> failwith "BUG: tc_exp.ml - Type-checking for mismatched arguments reached unexpected location."
	in tctl_rec _sl _tl

	(* expression type-checking *)

let tc_const (c: const): g_type = match c with
	IConst _ -> intTy
	| FConst _ -> floatTy
	| SConst _ -> stringTy
	| BConst _ -> boolTy
	| LConst _ -> longTy
	| KConst _ -> failwith "BUG: tc_const.ml - Type lookup performed directly on key constant."

let rec tc_exp (env: type_env) (e: r_exp): (gen_exp * g_type) tc_res = match e with
	ConstExp(KConst k, _) -> Valid (ConstExpC(KConst k), keyTy)
	| ConstExp(c, _) -> Valid (ConstExpC c, tc_const c)
	| VarExp(_, x, _) -> (match StringMap.find_opt x env.localIds with
		Some tau -> Valid (VarExpC x, tau)
		| _ -> (match Hashtbl.find_opt env.globalIds x with
			Some tau -> Valid (VarExpC x, tau)
			| _ -> failwith "BUG: tc_exp.ml - Failed variable lookup during type-checking phase."
		)
	)
	| OpExp(_, _) -> failwith "BUG: tc_exp.ml - Operator expression in non-application position."
	| TupleExp(ctor, el, p) ->
		let* et_l' = tc_exp_list env el in (match ctor with
			None ->
				let tau_s = TupleTy(List.map snd et_l') in
				Valid (TupleExpC(get_box_id_tenv env, tau_s, List.map fst et_l'), tau_s)
			| Some (_, cx) -> (match Hashtbl.find_opt env.globalTIds cx with
				Some (TcCtor c) ->
					let tau_s = NamedTy(CT, c) in
					Valid (TagTupleExpC(get_box_id_tenv env, tau_s, cx, List.map fst et_l'), tau_s)
				| _ -> Error (NonCtor_Err(cx, p))
			)
		)
	| DataArrayExp(i, tau_o, dim_l, el, p) ->
		let* et_l' = tc_exp_list env el in
		let tau = (match tau_o with None -> snd (List.hd et_l') | Some tau -> tau) in
		let dim_prod = List.fold_left (fun i p -> i * p) 1 dim_l in
		if dim_prod <> List.length el then Error (MismatchedArrayDim_Err (dim_l, dim_prod, List.length el, p))
		else Valid (NewArrayExpC(List.map (fun i -> ConstExpC (IConst i)) dim_l, List.map fst et_l', tau), ArrayTy(i, tau))
	| FormatArrayExp(_, _, _, p) -> Error (NestedFormat_Err p)
	(*| FormatArrayExp(i, dim_l, e, _) ->
		let* dt_l' = tc_exp_list env dim_l in
		let* (e', tau) = tc_exp env e in
		Valid (FormatArrayExpC(List.map fst dt_l', e', tau), ArrayTy(i, tau))*)
	| NewStructExp(_, cx, fl, p) ->
		let* ftl' = map_try_res (fun (x, e) -> let* (e', t) = tc_exp env e in Valid (x, e', t)) fl in
		let* el' = (match Hashtbl.find_opt env.globalTIds cx with
			Some (TcTDef (StructTD pl)) ->
				map_try_res (fun (x, _) ->
					match List.find_opt (fun (y, _, _) -> x = y) ftl' with
						None -> Error (MissingField_Err(cx, x, p))
						| Some (_, e', _) -> Valid e' 
				) pl
			| Some _ -> Error (BadCtorStruct_Err(cx, p))
			| _ -> Error (NonCtor_Err(cx, p))
		) in Valid (NewStructExpC(cx, el'), NamedTy(CT, cx))
	| AppExp(ef, el, p) ->
		let* et_l' = tc_exp_list env el in
		let tau_al = List.map snd et_l' in
		let el' = List.map fst et_l' in
		let* (f, d, (tau_pl, tau_rn)) = tc_fun_exp env ef (hd_opt tau_al) in
		let* _ = tc_type_list tau_al tau_pl p in (match d with
			UnaryGF fsm -> Valid (UnaryExpC(fsm, List.nth el' 0), tau_rn)
			| BinaryGF fsm -> Valid (BinExpC(fsm, List.nth el' 0, List.nth el' 1), tau_rn)
			| InternalGF fsm ->	let* c = calc_cfun env fsm el' p in Valid (c, tau_rn)
			| TupleIndexGF i -> Valid (TupleIndexExpC(List.hd el', i, List.hd tau_pl), tau_rn)
			| ArrayIndexGF rw ->
				let (rw', tau_r', et') =
					if rw = RR then (RC, tau_rn, List.tl el')
					else (WC (List.nth el' 1), unitTy, List.tl (List.tl el')) in
				Valid (ArrayIndexExpC(rw', List.hd el', FullIndexC et', tau_rn), tau_r')
			| ArrayLengthGF -> Valid (ArrayLengthExpC (List.hd el'), tau_rn)
			| ArrayDimsGF i -> Valid (ArrayDimsExpC(i, List.hd el'), tau_rn)
			| StructFieldGF(rw, i, cx) ->
				let rw' = if rw = RR then RC else WC (List.nth el' 1) in
				Valid (StructFieldExpC(rw', List.nth el' 0, i, cx), tau_rn)
			| CallGF _ ->
				(*let elx = List.mapi (fun i (e', tau_a) -> if List.mem i vl then BoxExpC(get_box_id_tenv env, e', tau_a) else e') et_l' in
				*)
				let elx = List.map (fun (e', _) -> e') et_l' in
				Valid (CallExpC(VarExpC f, elx, tau_rn), tau_rn)
		)
and tc_fun_exp (env: type_env) (ef: r_exp) (tau_a: g_type option): (string * g_fun * canon_tag fun_type) tc_res = match ef with
	VarExp(_, f, p) -> (match lookup_fun_tenv env f tau_a with
		Some (f, (d, tau_f)) -> (match d with
			UnaryASMSym fsm -> Valid (f, UnaryGF fsm, tau_f)
			| BinaryASMSym fsm -> Valid (f, BinaryGF fsm, tau_f)
			| InternalSym fsm -> Valid (f, InternalGF fsm, tau_f)
			| ExternalSym vl -> Valid (f, CallGF vl, tau_f)
			| _ -> Valid (f, CallGF [], tau_f)
		)
		| None -> Error (NoOverload_Err(f, tau_a, p))
	)
	| OpExp(TupleIndexOp i, p) -> (match tau_a with
		None -> failwith "BUG: tc_exp.ml - No argument for tuple index operation."
		| Some (TupleTy tau_l) -> (match index_type_list tau_l (i - 1) with
			None -> Error (TupleIndexOOB_Err(TupleTy tau_l, i, p))
			| Some tau_i ->	Valid ("", TupleIndexGF (i - 1), ([TupleTy tau_l], tau_i))
		)
		| Some tau -> Error (NonTuple_Err(tau, p))
	)
	| OpExp(ArrayIndexOp rw, p) -> (match tau_a with
		None -> failwith "BUG: tc_exp.ml - No argument for array index operation."
		| Some (ArrayTy(i, tau_v)) ->
				(* strictly speaking, it should return unit for the WRITE case,
					but we need the inner type for code-gen *)
			let tau_i = List.init i (fun _ -> intTy) in
			let tau_il = if rw = RR then tau_i else tau_v :: tau_i in
			Valid ("", ArrayIndexGF rw, (ArrayTy(i, tau_v) :: tau_il, tau_v))
		| Some _ ->
			let fName = if rw = RR then "_builtin_lookup" else "_builtin_update" in 
			tc_fun_exp env (VarExp(CT, fName, p)) tau_a
	)
	| OpExp(StructFieldOp(rw, x), p) -> (match tau_a with
		None -> failwith "BUG: tc_exp.ml - No argument for struct field operation."
		| Some (NamedTy(_, cx)) -> (match Hashtbl.find_opt env.globalTIds cx with
				(* check field *)
			Some (TcTDef (StructTD fl)) -> (match lookup_field_list fl x with
				None -> Error (BadField_Err(cx, x, p))
				| Some (tau, i) ->
					(* read / write case *)
					let (tau_args, tau_r) =
						if rw = RR then ([NamedTy(CT, cx)], tau)
						else ([NamedTy(CT, cx); tau], unitTy)
					in Valid ("", StructFieldGF(rw, i, cx), (tau_args, tau_r))
			)
			| _ -> Error (NonStruct_Err(NamedTy(CT, cx), p))
		)
		| Some tau -> Error (NonStruct_Err(tau, p))
	)
	| OpExp(MeasureOp, p) -> (match tau_a with
		None -> failwith "BUG: tc_exp.ml - No argument for measure operation."
		| Some (ArrayTy(i, tau_v)) ->
			if i = 1 then Valid ("", ArrayLengthGF, ([ArrayTy(i, tau_v)], intTy))
			else Valid ("", ArrayDimsGF i, ([ArrayTy(i, tau_v)], TupleTy (List.init i (fun _ -> intTy))))
		| Some _ -> tc_fun_exp env (VarExp(CT, "_builtin_measure", p)) tau_a
	)
	| _ -> failwith "UNIMPLEMENTED: tc_exp.ml - function non-var case."
and tc_exp_list (env: type_env) (el: r_exp list): ((gen_exp * g_type) list) tc_res = match el with
	[] -> Valid []
	| e :: et ->
		let* res = tc_exp env e in
		let* res_t = tc_exp_list env et in Valid (res :: res_t)

let tc_extra_exp (env: type_env) (e: r_exp) (x: string): (gen_exp * g_type * gen_stmt list) tc_res = match e with
	FormatArrayExp(i, dim_l, e, _) ->
		let* dt_l' = tc_exp_list env dim_l in
		let* (e', tau) = tc_exp env e in
		let b = [
			VarStmtC("__i", ConstExpC (IConst 0), intTy);
			WhileStmtC(BinExpC("ilt", VarExpC "__i", ArrayLengthExpC (VarExpC x)), [
				EvalStmtC (ArrayIndexExpC(WC e', VarExpC x, RawIndexC (VarExpC "__i"), tau));
				AssignStmtC("__i", BinExpC("iadd", VarExpC "__i", ConstExpC (IConst 1)))
			])
		] in Valid (NewArrayExpC(List.map fst dt_l', [], tau), ArrayTy(i, tau), b)
	| _ -> let* (e', tau) = tc_exp env e in Valid (e', tau, [])

	(* statement type-checking *)

let rec tc_stmt (cont: fun_cont) (env: type_env) (s: r_stmt): (type_env * gen_stmt list * bool) tc_res = match s with
	EvalStmt(e, _) ->
		let* (e', _) = tc_exp env e in Valid (env, [EvalStmtC e'], false)
	| AssignStmt(x, e, _) -> (match StringMap.find_opt x env.localIds with
		Some _ -> let* (e', _) = tc_exp env e in Valid (env, [AssignStmtC(x, e')], false)
		| _ -> (match Hashtbl.find_opt env.globalIds x with
			Some _ -> let* (e', _) = tc_exp env e in Valid (env, [AssignStmtC(x, e')], false)
			| _ -> dump_tenv env; failwith ("BUG: tc_exp.ml - Failed assignment variable \"" ^
				x ^ "\" lookup during type-checking phase.")
		)
	)
	| ReturnStmt(eo, _) -> (match eo with
		None -> Valid (env, [ReturnStmtC None], true)
		| Some e -> let* (e', _) = tc_exp env e in Valid (env, [ReturnStmtC (Some e')], true)
	)
	| PatStmt(px, e, _) -> (match px with
		VarPat x ->
			let* (e', tau_e, b) = tc_extra_exp env e x in 
			let ef = if cont.lf = Lin && is_heap_type env tau_e then GCNewRootExpC e' else e' in
			Valid ({ env with localIds = StringMap.add x tau_e env.localIds }, (VarStmtC(x, ef, tau_e)) :: b, false)
		| _ -> failwith "Unimplemented: tc_exp.ml - Patterns.")
	| IfStmt(ec, b1, b2, _) ->
		let* (ec', _) = tc_exp env ec in 
		let* (_, b1', term1) = tc_body (nonLinCont cont) env b1 in
		let* (_, b2', term2) = tc_body (nonLinCont cont) env b2 in Valid (env, [IfStmtC(ec', b1', term1, b2', term2)], term1 && term2)
	| WhileStmt(ec, b, _) ->
		let* (ec', _) = tc_exp env ec in
		let* (_, b', _) = tc_body (nonLinCont cont) env b in Valid (env, [WhileStmtC(ec', b')], false)
	| ForStmt(x, rt, e, b, _) ->
		let* (e', _) = tc_exp env e in
		let* (tau_x, cmp, _) = (match rt with
			LtRange -> Valid (intTy, "ilt", false)
			| LeqRange -> Valid (intTy, "ileq", false)
			| ListRange -> failwith "UNIMPLEMENTED: tc_exp.ml - List case for for loop."
			(*| ListRange -> (match tau_e with
				ArrayTy(_, tau_a) -> Valid (tau_a, "ilt", true)
				| _ -> Error (NonArrayType_Err(simplify_type tau_e, p))
			)*)
		) in let env' = { env with localIds = StringMap.add x tau_x env.localIds } in
			(* for loop header:  _iterator, conditional depending on list case *)
		let (i', end') = (x, e') (*if list_flag then ("__i", LengthExpC e') else (x, e') *) in
		let x' = VarExpC i' in
		let cond' = BinExpC(cmp, x', end') in
			(* for loop body: body; _iterator = _iterator + 1 *)
		let* (_, b', _) = tc_body (nonLinCont cont) env' b in
		let b'' = b' @ [AssignStmtC(i', BinExpC("iadd", x', ConstExpC (IConst 1)))] in
			(* for loop body: prefix with x = e[_iterator] for list case *)
		(*let bf' = if list_flag then AssignStmtC(i', ArrayIndexExpC(e', [x'], tau_x)) :: b'' else b'' in*)
		Valid (env, [VarStmtC(i', ConstExpC (IConst 0), intTy); WhileStmtC(cond', b'')], false)
	| GCCollectStmt _ -> Valid (env, [GCCollectStmtC], false)
and tc_body (cont: fun_cont) (env: type_env) (b: r_stmt list): (type_env * gen_stmt list * bool) tc_res = match b with
	[] -> Valid (env, [], false)
	| s :: st ->
		let* (env2, s', term0) = tc_stmt cont env s in (match st with
			[] -> Valid (env2, s', term0)
			| _ ->
				if term0 then Error (EarlyReturn_Err (cont.f, ann_stmt s))
				else let* (env3, st', termX) = tc_body cont env2 st in Valid (env3, s' @ st', termX)
		)
		
	(* declaration / sectional type-checking *)

let rec add_param_list (env: type_env) (pl: (string * g_type) list): type_env = match pl with
	[] -> env
	| (x, tau) :: pt ->
		add_param_list { env with localIds = StringMap.add x tau env.localIds } pt

let tc_dec (env: type_env) (d: r_dec): ((string * gen_dec) list) tc_res = match d with
	FunDec(Method(lf, f, pl, tau_r, b), p) ->
		let tau_pl = List.map (fun (_, tau) -> tau) pl in
			(* uses non-type-canon function name (to allow for overloads on lookup) *)
		add_fun_tenv env f (UserDefSym, (tau_pl, tau_r));
		let localEnv = add_param_list env pl in
			(* use canon name for error messages + final declaration *)
		let fName = "_" ^ (tag_of_type (hd_opt tau_pl)) ^ f in
		let* (_, b', term) = tc_body { f = fName; lf = lf; } localEnv b in
		if not term then (
			if tau_r <> unitTy then Error (NoReturn_Err(fName, p))
			else Valid [(fName, FunDecC (MethodC(pl, tau_r, b' @ [ReturnStmtC None])))]
		) else Valid [(fName, FunDecC (MethodC(pl, tau_r, b')))]
	| TDefDec(x, td, _) -> Hashtbl.add env.globalTIds x (TcTDef td); Valid [(x, TDefDecC td)]
	| GlobalDec(cf, x, e, _) ->
		let* (e', tau) = tc_exp env e in
		let* ef = calc_exp env e' in
		Hashtbl.add env.globalIds x tau;
		Valid [(x, GlobalDecC(cf = CDec, ef))]

let tc_section (env: type_env) (SectionR dl: r_section): ((string * gen_dec) list) tc_res =
	let rec tcs_rec dl = match dl with
		[] -> Valid []
		| d :: dt ->
			let* d' = tc_dec env d in
			let* dt' = tcs_rec dt in Valid (d' @ dt')
	in tcs_rec dl
