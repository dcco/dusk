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
		(* tuples *)
	| NonTuple_Err of g_type * l_pos
	| TupleIndexOOB_Err of g_type * int * l_pos
		(* tag tuples *)
	| NonCtor_Err of string * l_pos
		(* structs *)
	| NonStruct_Err of g_type * l_pos
	| BadCtorStruct_Err of string * l_pos
	| MissingField_Err of string * string * l_pos
	| BadField_Err of string * string * l_pos
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
	| NonTuple_Err(t, p) -> "Tuple operation called on non-tuple type \"" ^ (string_of_type t) ^ "\" at " ^ (string_of_pos p) ^ "."
	| TupleIndexOOB_Err(t, i, p) -> "Attempted to access index " ^ (string_of_int i ) ^ " of tuple type \"" ^
		(string_of_type t) ^ "\" at " ^ (string_of_pos p) ^ "."
	| NonCtor_Err(t, p) -> "Type name \"" ^ t ^ "\" did not resolve to a constructor at " ^ (string_of_pos p) ^ "."
	| NonStruct_Err(t, p) -> "Struct operation called on non-struct type \"" ^ (string_of_type t) ^ "\" at " ^ (string_of_pos p) ^ "."
	| BadCtorStruct_Err(t, p) -> "Attempted to initialize struct with non-struct constructor \"" ^ t ^ "\" at " ^ (string_of_pos p) ^ "."
	| MissingField_Err(t, x, p) -> "Initialization of struct \"" ^ t ^
		"\" missing field \"" ^ x ^ "\" at " ^ (string_of_pos p) ^ "."
	| BadField_Err(t, x, p) -> "Attempted to read non-existent field \"" ^ x ^
		"\" from struct \"" ^ t ^ "\" at " ^ (string_of_pos p) ^ "."
	| EarlyReturn_Err(f, p) ->
		"Early return from function \"" ^ f ^ "\" producing unreachable code at " ^ (string_of_pos p) ^ "."
	| NoReturn_Err(f, p) ->
		"Declaration of function \"" ^ f ^ "\" does not return on all paths at " ^ (string_of_pos p) ^ "."
	
	(* type-checking auxiliaries *)

type g_fun =
	BinaryGF of string
	| TupleIndexGF of int
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

	(* expression type-checking *)

let tc_const (c: const): g_type = match c with
	IConst _ -> intTy
	| FConst _ -> floatTy
	| SConst _ -> stringTy
	| BConst _ -> boolTy

let rec tc_exp (env: type_env) (e: r_exp): (gen_exp * g_type) tc_res = match e with
	ConstExp(c, _) -> Valid (ConstExpC c, tc_const c)
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
			None ->	Valid (TupleExpC (List.map fst et_l'), TupleTy(List.map snd et_l'))
			| Some (_, cx) -> (match Hashtbl.find_opt env.globalTIds cx with
				Some (TcCtor c) -> Valid (TagTupleExpC(cx, List.map fst et_l'), NamedTy(CT, c))
				| _ -> Error (NonCtor_Err(cx, p))
			)
		)
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
		) in Valid (NewStructExpC(None, el'), NamedTy(CT, cx))
	| AppExp(ef, el, _) ->
		let* et_l' = tc_exp_list env el in
		let tau_al = List.map snd et_l' in
		let el' = List.map fst et_l' in
		let* (f, d, (_, tau_r)) = tc_fun_exp env ef (hd_opt tau_al) in (match d with
			BinaryGF fsm -> Valid (BinExpC(fsm, List.nth el' 0, List.nth el' 1), tau_r)
			| TupleIndexGF i -> Valid (TupleIndexExpC(List.hd el', i, tau_r), tau_r)
			| StructFieldGF(rw, i, cx) ->
				let rw' = if rw = RR then RC else WC (List.nth el' 1) in
				Valid (StructFieldExpC(rw', List.nth el' 0, i, cx), tau_r)
			| CallGF vl ->
				let elx = List.mapi (fun i (e', tau_a) -> if List.mem i vl then BoxExpC(get_box_id_tenv env, e', tau_a) else e') et_l' in
				Valid (CallExpC(VarExpC f, elx, tau_r), tau_r)
		)
	| _ -> failwith "UNIMPLEMENTED: tc_exp.ml - type-checking exp case."
and tc_fun_exp (env: type_env) (ef: r_exp) (tau_a: g_type option): (string * g_fun * canon_tag fun_type) tc_res = match ef with
	VarExp(_, f, p) -> (match lookup_fun_tenv env f tau_a with
		Some (f, (d, tau_f)) -> (match d with
			BinaryASMSym fsm -> Valid (f, BinaryGF fsm, tau_f)
			| ExternalSym vl -> Valid (f, CallGF vl, tau_f)
			| _ -> Valid (f, CallGF [], tau_f)
		)
		| None -> Error (NoOverload_Err(f, tau_a, p))
	)
	| OpExp(TupleIndexOp i, p) -> (match tau_a with
		None -> failwith "BUG: tc_exp.ml - No argument for tuple index operation."
		| Some (TupleTy tau_l) -> (match index_type_list tau_l (i - 1) with
			None -> Error (TupleIndexOOB_Err(TupleTy tau_l, i, p))
			| Some tau_i -> Valid ("", TupleIndexGF (i - 1), ([TupleTy tau_l], tau_i))
		)
		| Some tau -> Error (NonTuple_Err(tau, p))
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
	| _ -> failwith "UNIMPLEMENTED: tc_exp.ml - function non-var case."
and tc_exp_list (env: type_env) (el: r_exp list): ((gen_exp * g_type) list) tc_res = match el with
	[] -> Valid []
	| e :: et ->
		let* res = tc_exp env e in
		let* res_t = tc_exp_list env et in Valid (res :: res_t)

	(* statement type-checking *)

let rec tc_stmt (f: string) (env: type_env) (s: r_stmt): (type_env * gen_stmt list * bool) tc_res = match s with
	EvalStmt(e, _) ->
		let* (e', _) = tc_exp env e in Valid (env, [EvalStmtC e'], false)
	| AssignStmt(x, e, _) -> (match StringMap.find_opt x env.localIds with
		Some _ -> let* (e', _) = tc_exp env e in Valid (env, [AssignStmtC(x, e')], false)
		| _ -> failwith "BUG: tc_exp.ml - Failed variable lookup during type-checking phase."
	)
	| PatStmt(px, e, _) ->
		let* (e', tau_e) = tc_exp env e in (match (px, tau_e) with
			(VarPat x, _) ->
				Valid ({ env with localIds = StringMap.add x tau_e env.localIds }, [VarStmtC(x, e', tau_e)], false)
			| _ -> failwith "Unimplemented: res_out.ml - Patterns.")
	| IfStmt(ec, b1, b2, _) ->
		let* (ec', _) = tc_exp env ec in 
		let* (_, b1', term1) = tc_body f env b1 in
		let* (_, b2', term2) = tc_body f env b2 in Valid (env, [IfStmtC(ec', b1', term1, b2', term2)], false)
	| WhileStmt(ec, b, _) ->
		let* (ec', _) = tc_exp env ec in 
		let* (_, b', _) = tc_body f env b in Valid (env, [WhileStmtC(ec', b')], false)
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
		let* (_, b', _) = tc_body f env' b in
		let b'' = b' @ [AssignStmtC(i', BinExpC("iadd", x', ConstExpC (IConst 1)))] in
			(* for loop body: prefix with x = e[_iterator] for list case *)
		(*let bf' = if list_flag then AssignStmtC(i', ArrayIndexExpC(e', [x'], tau_x)) :: b'' else b'' in*)
		Valid (env, [VarStmtC(i', ConstExpC (IConst 0), intTy); WhileStmtC(cond', b'')], false)
	| _ -> failwith "UNIMPLEMENTED: tc_exp.ml - statement case."
and tc_body (f: string) (env: type_env) (b: r_stmt list): (type_env * gen_stmt list * bool) tc_res = match b with
	[] -> Valid (env, [], false)
	| s :: st ->
		let* (env2, s', term0) = tc_stmt f env s in
		if term0 then Error (EarlyReturn_Err (f, ann_stmt s))
		else let* (env3, st', termX) = tc_body f env2 st in Valid (env3, s' @ st', termX)

	(* declaration / sectional type-checking *)

let rec add_param_list (env: type_env) (pl: (string * g_type) list): type_env = match pl with
	[] -> env
	| (x, tau) :: pt ->
		add_param_list { env with localIds = StringMap.add x tau env.localIds } pt

let tc_dec (env: type_env) (d: r_dec): ((string * gen_dec) list) tc_res = match d with
	FunDec(Method(f, pl, tau_r, b), p) ->
		let tau_pl = List.map (fun (_, tau) -> tau) pl in
			(* a function name cant be fully "canonized" until this stage *)
		let fName = "_" ^ (tag_of_type (hd_opt tau_pl)) ^ f in
		add_fun_tenv env fName (UserDefSym, (tau_pl, tau_r));
		(*let env' = { env with local = StringMap.add fName (FunTy(tau_pl', tau_r')) } in*)
		let localEnv = add_param_list env pl in
		let* (_, b', term) = tc_body fName localEnv b in
		if not term then (
			if tau_r <> unitTy then Error (NoReturn_Err(fName, p))
			else Valid [(fName, FunDecC (MethodC(pl, tau_r, b' @ [ReturnStmtC None])))]
		) else Valid [(fName, FunDecC (MethodC(pl, tau_r, b')))]
	| TDefDec(x, td, _) -> Hashtbl.add env.globalTIds x (TcTDef td); Valid [(x, TDefDecC td)]

let tc_section (env: type_env) (SectionR dl: r_section): ((string * gen_dec) list) tc_res =
	let rec tcs_rec dl = match dl with
		[] -> Valid []
		| d :: dt ->
			let* d' = tc_dec env d in
			let* dt' = tcs_rec dt in Valid (d' @ dt')
	in tcs_rec dl
