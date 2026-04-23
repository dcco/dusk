open Llvm
open Llvm_analysis
open Llvm_target

open Parser.Dusk_type
open Parser.Dusk_ast
open Builtin
open Fin_type
open Fin_ast
open Gen_cont
open Gen_type

	(* string literal generation *)

let genStrLit (cont: llvm_cont) (env: dusk_env) (s: string): dusk_val =
		(* create (pointer to) string constant data *)
	let i = genRef cont in
	let strVal = const_string context (s ^ "\x00") in
	(*let strRef = define_global ("_s" ^ (string_of_int i)) strVal (cont.llmod) in*)
		(* create (pointer to) struct containing string + meta data *)
	let structVal = const_struct context
		[| const_int iType (String.length s); strVal |] in
	let structRef = define_global ("_sz" ^ (string_of_int i)) structVal cont.llmod in
	let v = (structRef, ptrType) in 
	Hashtbl.add env (DStrLit s) (DVal(v, None)); v

	(* expression generation *)

let genConst (cont: llvm_cont) (env: dusk_env) (c: const): dusk_val = match c with
	IConst i -> (const_int iType i, iType)
	| FConst f -> (const_float fType f, fType)
	| BConst b -> (const_int bType (if b then 1 else 0), bType)
	| SConst s -> (match Hashtbl.find_opt env (DStrLit s) with
		Some (DVal (v, _)) -> v
		| _ -> genStrLit cont env s
	)
	| LConst l -> (const_of_int64 i64Type l true, i64Type)
	| KConst k -> (match Hashtbl.find_opt env (DKeyLit k) with
		Some (DVal ((v, _), _)) -> (build_load iType v "_kT" cont.builder, iType)
		| _ ->
			let v = declare_global iType ("K_" ^ k) cont.llmod in
			Hashtbl.add env (DKeyLit k) (DVal((v, iType), None));
			(build_load iType v "_kT" cont.builder, iType)
	)

		(* failwith "BUG: gen_exp.ml - Raw string literal in generation phase." *)

let rec genProduct (cont: llvm_cont) (vl: llvalue list): llvalue = match vl with
	[] -> failwith ("BUG: gen_exp.ml - Attempted to multiply empty list of values.")
	| [v] -> v
	| v :: vt -> let vt' = genProduct cont vt in build_mul v vt' "_mulT" cont.builder

let genIndexProd (cont: llvm_cont) (va: llvalue) (vl: llvalue list) (dim: int): llvalue =
	let dimsPtr = build_gep gcArrType va (Array.of_list [const_int iType 1]) "_dimsPT" cont.builder in
	let rec gip_rec vl i = match vl with
		[] -> failwith ("BUG: gen_exp.ml - Empty index list found while generating index product.")
		| [v] -> v
		| v :: vt ->
			let vt' = gip_rec vt (i + 1) in
			let sx = "_dim" ^ (string_of_int i) in
			let sizePtr = build_gep (gcDimsType dim) dimsPtr 
				(Array.of_list [const_int iType 0; const_int iType i]) (sx ^ "PT") cont.builder in
			let v_size = build_load iType sizePtr (sx ^ "T") cont.builder in
			let vm = build_mul vt' v_size "_mulT" cont.builder in
			build_add v vm "_addT" cont.builder
	in gip_rec vl 0

let genBoxStore (cont: llvm_cont) (env: dusk_env) (boxId: int) (vx: llvalue): dusk_val =
	match Hashtbl.find_opt env (DBox boxId) with
		Some (DVal((vb, tb), alignOpt)) ->
			let vs = build_store vx vb cont.builder in
			Option.iter (fun align -> set_alignment align vs) alignOpt; (vb, tb)
		| _ -> failwith "BUG: gen_exp.ml - Ungenerated box encountered in generation phase."

let genVar (cont: llvm_cont) (env: dusk_env) (k: dusk_key) (name: string): dusk_val = match Hashtbl.find_opt env k with
	Some (DFunVal (v, t)) -> (v, t)
	| Some (DVal ((v, t), alignOpt)) ->
		let vx = build_load t v name cont.builder in
		Option.iter (fun align -> set_alignment align vx) alignOpt; (vx, t)
	| Some (DGlobal (p, t)) -> let v = build_load t p name cont.builder in (v, t)
	| Some _ -> failwith ("BUG: gen_exp.ml - Variable \"" ^ name ^ "\" resolved to non-value.")
	| None -> failwith ("BUG: gen_exp.ml - Unexpected variable \"" ^ name ^ "\" encountered in generation phase.")

let rec genExp (cont: llvm_cont) (env: dusk_env) (e: gen_exp): dusk_val = let bx = cont.builder in match e with
	ConstExpC c -> genConst cont env c
	(*| LitExpC i -> (match Hashtbl.find env (DLitId i) with
		DVal (v, t) -> (v, t)
		| _ -> failwith ("BUG: gen_exp.ml - String literal incorrectly resolved in generation phase.")
	)*)
	| NullExpC -> (const_null ptrType, ptrType)
	| VarExpC x -> genVar cont env (DVar x) x 
	| UnaryExpC(xOp, e) ->
		let (v, _) = genExp cont env e in (match xOp with
			"ineg" -> (build_neg v "_negT" bx, iType)
			| "fneg" -> (build_fneg v "_negT" bx, fType)
			| "bnot" -> (build_not v "_notT" bx, bType)
			| "ftoi" -> (build_fptosi v iType "_castT" bx, iType)
			| "i64toi" -> (build_trunc v iType "_castT" bx, iType)
			| "itof" -> (build_sitofp v fType "_castT" bx, fType)
			| "itoi64" -> (build_sext v i64Type "_castT" bx, i64Type)
			| _ -> failwith ("BUG: gen_exp.ml - Unexpected operator \"" ^ xOp ^ "\" encountered in generation phase.")
		)
	| BinExpC(xOp, e1, e2) ->
		let (v1, _) = genExp cont env e1 in
		let (v2, _) = genExp cont env e2 in (match xOp with
			"iadd" -> (build_add v1 v2 "_addT" bx, iType)
			| "isub" -> (build_sub v1 v2 "_subT" bx, iType)
			| "imul" -> (build_mul v1 v2 "_mulT" bx, iType)
			| "idiv" -> (build_sdiv v1 v2 "_divT" bx, iType)
			| "imod" -> (build_srem v1 v2 "_modT" bx, iType)
			| "fadd" -> (build_fadd v1 v2 "_addT" bx, fType)
			| "fsub" -> (build_fsub v1 v2 "_subT" bx, fType)
			| "fmul" -> (build_fmul v1 v2 "_mulT" bx, fType)
			| "fdiv" -> (build_fdiv v1 v2 "_divT" bx, fType)
			| "ieq" -> (build_icmp Icmp.Eq v1 v2 "_cmpT" bx, bType)
			| "ineq" -> (build_icmp Icmp.Ne v1 v2 "_cmpT" bx, bType)
			| "ileq" -> (build_icmp Icmp.Sle v1 v2 "_cmpT" bx, bType)
			| "ilt" -> (build_icmp Icmp.Slt v1 v2 "_cmpT" bx, bType)
			| "igeq" -> (build_icmp Icmp.Sge v1 v2 "_cmpT" bx, bType)
			| "igt" -> (build_icmp Icmp.Sgt v1 v2 "_cmpT" bx, bType)
			| "feq" -> (build_fcmp Fcmp.Oeq v1 v2 "_cmpT" bx, bType)
			| "fneq" -> (build_fcmp Fcmp.One v1 v2 "_cmpT" bx, bType)
			| "fleq" -> (build_fcmp Fcmp.Ole v1 v2 "_cmpT" bx, bType)
			| "flt" -> (build_fcmp Fcmp.Olt v1 v2 "_cmpT" bx, bType)
			| "fgeq" -> (build_fcmp Fcmp.Oge v1 v2 "_cmpT" bx, bType)
			| "fgt" -> (build_fcmp Fcmp.Ogt v1 v2 "_cmpT" bx, bType)
			| "band" -> (build_and v1 v2 "_andT" bx, bType)
			| "bor" -> (build_or v1 v2 "_orT" bx, bType)
			| "i64add" -> (build_add v1 v2 "_addT" bx, i64Type)
			| "i64sub" -> (build_sub v1 v2 "_subT" bx, i64Type)
			| "i64mul" -> (build_mul v1 v2 "_mulT" bx, i64Type)
			| "i64div" -> (build_sdiv v1 v2 "_divT" bx, i64Type)
			| _ -> failwith ("BUG: gen_exp.ml - Unexpected operator \"" ^ xOp ^ "\" encountered in generation phase.")
		)
	| CallExpC(ef, el, tau_r) ->
		let (vf, tf) = genExp cont env ef in
		let vl = List.map (fun e -> fst (genExp cont env e)) el in
		(build_call tf vf (Array.of_list vl) "" cont.builder, genType tau_r)
	(*| BoxExpC(i, e, _) ->
		let (ve, _) = genExp cont env e in (match Hashtbl.find_opt env (DBox i) with
			Some (DVal ((vb, tb), alignOpt)) ->
				let vs = build_store ve vb bx in
				Option.iter (fun align -> set_alignment align vs) alignOpt; (vb, tb)
			| _ -> failwith "BUG: gen_exp.ml - Ungenerated box encountered in generation phase."
		)
	| BoxExpC(_, _, _) -> failwith "BUG: gen_exp.ml - Box expression encountered (currently unused feature.)"*)
	| TupleExpC(boxId, _, el) ->
		 	(* compile the sub-expressions *)
		let res_l = List.map (fun e -> genExp cont env e) el in
		let tau_l = List.map snd res_l in
			(* initialize struct + fields *)
		let tau_enum = structType tau_l in
		let (sVal, _) = List.fold_left (fun (sVal, i) (v, _) ->
			let sVal' = build_insertvalue sVal v i "_stT" bx in (sVal', i + 1)
		) (undef tau_enum, 0) res_l in
		genBoxStore cont env boxId sVal
	| TagTupleExpC(boxId, _, tag, el) ->
			(* find the tag literal *)
		let tagLit = (match Hashtbl.find_opt env (DCtor tag) with
			Some (DEnum i) -> const_int i8Type i
			| Some (DGlobal (p, _)) -> build_load i8Type p "_G" bx
			| Some _ -> failwith ("BUG: gen_out.ml - Enum constructor \"" ^ tag ^ "\" resolved to non-enum constructor.")
			| None -> failwith ("BUG: gen_out.ml - Unexpected enum \"" ^ tag ^ "\" encountered in generation phase.")
		) in
		 	(* compile the sub-expressions *)
		let res_l = List.map (fun e -> genExp cont env e) el in
		let tau_l = List.map snd res_l in
			(* initialize struct + fields *)
		let tau_enum = structType (i8Type :: tau_l) in
		let sVal0 = build_insertvalue (undef tau_enum) tagLit 0 "_stT" bx in
		let (sVal, _) = List.fold_left (fun (sVal, i) (v, _) ->
			let sVal' = build_insertvalue sVal v (i + 1) "_stT" bx in (sVal', i + 1)
		) (sVal0, 0) res_l in
		genBoxStore cont env boxId sVal
	| TupleIndexExpC(ep, i, tau) ->
		let (vp, _) = genExp cont env ep in
		let t' = genInnerType env tau in
		let tv' = (struct_element_types t').(i) in
		let vPtr = build_gep t' vp (Array.of_list [const_int iType 0; const_int iType i]) "_elemPT" bx in
		let elem = build_load tv' vPtr "_elemT" bx in (elem, tv')
		(*let elem = build_extractvalue vp i "_elemT" bx in
		(elem, genType tau_v)*)
	| NewArrayExpC(dim_l, el, tau) ->
			(* calculate size *)
		let res_l = List.map (genExp cont env) dim_l in
		let dim = List.length res_l in
		let v_size = genProduct cont (List.map fst res_l) in
			(* initialize array *)
		let e_size = const_int iType (size_of_type cont (genType tau)) in
		let dim_size = const_int iType (if dim <= 1 then 0 else size_of_type cont (gcDimsType dim)) in
		let (new_arr, new_arr_type) = !(cont.gc).new_array in
		let nest_flag = const_int iType (if isHeapType env tau then 1 else 0) in
		let arrPtr = build_call new_arr_type new_arr (Array.of_list [e_size; v_size; dim_size; nest_flag]) "_arrPT" bx in
			(* initialize dimensions *)
		(if dim <= 1 then () else
			let dimsPtr = build_gep gcArrType arrPtr
				(Array.of_list [const_int iType 1]) "_dimsPT" bx in
			List.iteri (fun i vd ->
				let dx = "_dim" ^ (string_of_int i) ^ "PT" in
				let dimPtr = build_gep (gcDimsType dim) dimsPtr
					(Array.of_list [const_int iType 0; const_int iType i]) dx bx in
				ignore (build_store vd dimPtr bx)
			) (List.map fst res_l)
		);
			(* calculate array index pointer *)
		let t' = genType tau in
		let dataSlot = build_gep gcArrType arrPtr (Array.of_list [const_int iType 0; const_int iType 2]) "_dataS" bx in
		let dataPtr = build_load ptrType dataSlot "_dataPT" bx in
			(* store each value *)
		List.iteri (fun i e ->
			let eName = "_e" ^ (string_of_int i) ^ "PT" in
			let vPtr = build_gep t' dataPtr (Array.of_list [const_int iType i]) eName bx in
			let (vv, _) = genExp cont env e in
			ignore (build_store vv vPtr bx)
		) el; (arrPtr, ptrType)
	| ArrayIndexExpC(rw, ea, ix, tau) ->
			(* calculate index *)
		let (va, _) = genExp cont env ea in
		let vi = (match ix with
			RawIndexC e -> fst (genExp cont env e)
			| FullIndexC el ->
				let vl = List.map (fun e -> fst (genExp cont env e)) el in
				let dim = List.length vl in
				if dim = 1 then List.hd vl else genIndexProd cont va vl dim
		) in
			(* calculate array index pointer *)
		let t' = genType tau in
		let dataPtr = build_gep gcArrType va (Array.of_list [const_int iType 0; const_int iType 2]) "_dataS" bx in
		let v_data = build_load ptrType dataPtr "_dataPT" bx in
		let vPtr = build_gep t' v_data (Array.of_list [vi]) "_elemPT" bx in
			(* read / write to index *)
		(match rw with
			RC -> (build_load t' vPtr "_elemT" bx, t')
			| WC ev -> let (vv, _) = genExp cont env ev in (build_store vv vPtr bx, voidType)
		)
	| ArrayLengthExpC ea ->
		let (va, _) = genExp cont env ea in
		let sizePtr = build_gep gcArrType va (Array.of_list [const_int iType 0; const_int iType 1]) "_sizePT" bx in
		let vSize = build_load iType sizePtr "_sizeT" bx in (vSize, iType)
	| ArrayDimsExpC(i, ea) ->
		let (va, _) = genExp cont env ea in
		let dimsPtr = build_gep gcArrType va (Array.of_list [const_int iType 1]) "_dimsPT" bx in
		(dimsPtr, gcDimsType i)
	| NewStructExpC(tx, el) ->
		let res_l = List.map (genExp cont env) el in
		let tau_l = List.map snd res_l in
			(* lookup type information global *)
		let tc = (match Hashtbl.find_opt env (DTName tx) with
			Some (DTDef (StructTD_C(_, tc))) -> tc
			| _ -> failwith "BUG: gen_exp.ml - Bad type for struct initialization encountered in generation phase."
		) in
			(* heap allocate *)
		let tau_s = structType tau_l in
		let size = size_of_type cont tau_s in
		let (alloc_fun, alloc_type) = !(cont.gc).gc_alloc in
		let sPtr = build_call alloc_type alloc_fun (Array.of_list [const_int iType size; tc]) "_stPT" cont.builder in
			(* initialize struct *)
		List.iteri (fun i (ve, _) ->
			let vf = build_gep tau_s sPtr (Array.of_list [const_int iType 0; const_int iType i]) "_fiT" bx in
			ignore (build_store ve vf cont.builder)
		) res_l; (sPtr, ptrType)
	| StructFieldExpC(rw, e, i, cx) ->
		let (v, _) = genExp cont env e in
		let tau_l = (match Hashtbl.find_opt env (DTName cx) with
			Some (DTDef (StructTD_C(tau_l, _))) -> tau_l
			| _ -> failwith "BUG: gen_exp.ml - Bad type for struct field access encountered in generation phase."
		) in
		let tau_s = structType tau_l in
		let vf = build_gep tau_s v (Array.of_list [const_int iType 0; const_int iType i]) "_fieldPT" bx in
		(match rw with
			RC -> (build_load (List.nth tau_l i) vf "_fieldT" bx, List.nth tau_l i)
			| WC ev -> let (vv, _) = genExp cont env ev in (build_store vv vf bx, voidType)
		)
	| GCNewRootExpC e ->
		let (v, t) = genExp cont env e in
		let (new_root_fun, new_root_type) = !(cont.gc).gc_new_root in
		ignore (build_call new_root_type new_root_fun (Array.of_list [v]) "" bx); (v, t)
	| ConstArrayExpC(_, _, _) ->
		failwith "BUG: gen_exp.ml - Wrong generation function used for constant-only expression." 

	(* special expression generation that only gives constants *)

let rec genConstExp (cont: llvm_cont) (env: dusk_env) (e: gen_exp): dusk_val = (*let bx = cont.builder in*) match e with
	ConstExpC c -> genConst cont env c
	| NullExpC -> (const_null ptrType, ptrType)
	| ConstArrayExpC(dims, el, tau) ->
		let size = List.fold_left (fun i v -> i * v) 1 dims in
		let res_l = List.map (genConstExp cont env) el in
		let aVal = const_array (genType tau) (Array.of_list (List.map fst res_l)) in
		let rVal = define_global "_rawC" aVal cont.llmod in
		let sVal = const_struct context (Array.of_list ([
			const_int iType size;
			const_int iType size;
			rVal] @ (List.map (const_int iType) dims)
		)) in
		let xVal = define_global "_arrC" sVal cont.llmod in
		(xVal, ptrType) (* gcFullArrType (List.length dims)) *)
		(*let size = List.fold_left (fun i v -> i * v) 1 dims in*)
	| _ -> failwith "BUG: gen_exp.ml - Non-constant initializer for global declaration encountered in generation phase."

	(* statement generation *)

type blockInfo = int * llvalue

let addBlock ((i, fVal): blockInfo) (prefix: string): (llbasicblock * blockInfo) =
	(append_block context (prefix ^ "_" ^ (string_of_int i)) fVal, (i + 1, fVal))

let genAssign (cont: llvm_cont) (env: dusk_env) (x: string) (e: gen_exp): unit =
	let (ve, _) = genExp cont env e in
	(match Hashtbl.find env (DVar x) with
		DVal ((vx, _), alignOpt) ->
			let vs = build_store ve vx cont.builder in
			Option.iter (fun align -> set_alignment align vs) alignOpt
		| DGlobal (vx, _) -> ignore (build_store ve vx cont.builder)
		| _ -> failwith "BUG: gen_exp.ml - Unexpected assignment to non variable."
	)

let rec genStmt (cont: llvm_cont) (env: dusk_env) (b: blockInfo) (s: gen_stmt): blockInfo = match s with
	EvalStmtC e -> let _ = genExp cont env e in b
	| AssignStmtC(x, e) -> genAssign cont env x e; b
	| VarStmtC(x, e, _) -> genAssign cont env x e; b
	| ReturnStmtC rv -> let _ = (match rv with
		None -> build_ret_void (cont.builder)
		| Some e ->
			let (ve, _) = genExp cont env e in build_ret ve (cont.builder)) in b
	| IfStmtC(ec, body, term1, elseBody, term2) ->
		let (vc, _) = genExp cont env ec in
			(* create branch statement *)
		let (block1, b1) = addBlock b "body" in
		let (block2, b2) = addBlock b1 "else_body" in
		let _ = build_cond_br vc block1 block2 (cont.builder) in
			(* stub out final block when necessary *)
		let (blockF, bx) = if not (term1 && term2) then addBlock b2 "join" else (block2, b2) in
			(* generate block 1 + 2, join when relevant *)
		position_at_end block1 (cont.builder);
		let bx' = genBody cont env bx body in
		if not term1 then ignore (build_br blockF (cont.builder)) else ();
		position_at_end block2 (cont.builder);
		let bf = genBody cont env bx' elseBody in
		if not term2 then ignore (build_br blockF (cont.builder)) else ();
			(* re-position at final join *)
		position_at_end blockF (cont.builder); bf
	| WhileStmtC(ec, body) ->
			(* create loop starting block *)
		let (block1, b1) = addBlock b "cond" in
		ignore (build_br block1 (cont.builder));
		position_at_end block1 (cont.builder);
		let (vc, _) = genExp cont env ec in
			(* create branch statement *)
		let (block2, b2) = addBlock b1 "body" in 
		let (blockF, b3) = addBlock b2 "end" in 
		let _ = build_cond_br vc block2 blockF (cont.builder) in
			(* generate main block, branch to start *)
		position_at_end block2 (cont.builder);
		let bf = genBody cont env b3 body in
		ignore (build_br block1 (cont.builder));
		position_at_end blockF (cont.builder); bf
	| GCCollectStmtC ->
		let (collect_fun, collect_type) = !(cont.gc).gc_collect in
		ignore (build_call collect_type collect_fun (Array.of_list []) "" cont.builder); b

and genBody (cont: llvm_cont) (env: dusk_env) (b: blockInfo) (body: gen_stmt list): blockInfo = match body with
	[] -> b
	| s :: st -> let b' = genStmt cont env b s in genBody cont env b' st

	(* declaration generation *)

let genParamList (cont: llvm_cont) (env: dusk_env) (pl: (string * g_type) list) (v: llvalue): dusk_env =
	let env' = Hashtbl.copy env in
	let va = params v in
	let rec gpl_rec pl i = match pl with
		[] -> ()
		| (x, tau) :: pt ->
			let (vp, t, alignOpt) = (Array.get va i, genType tau, genAlign env tau) in
			let vx = build_alloca t ("_" ^ x) cont.builder in
			let vs = build_store vp vx cont.builder in
			Option.iter (fun align -> set_alignment align vx; set_alignment align vs) alignOpt;
			Hashtbl.add env' (DVar x) (DVal ((vx, t), alignOpt)); gpl_rec pt (i + 1)
	in gpl_rec pl 0; env'

let genPreAlloc (cont: llvm_cont) (env: dusk_env) (b: gen_stmt list): unit =
	let varList = collect_var_body b in
	List.iter (fun (x, tau) ->
		let t = genType tau in
		let alignOpt = genAlign env tau in
		let vx = build_alloca t ("_" ^ x) (cont.builder) in
		Option.iter (fun align -> set_alignment align vx) alignOpt;
		Hashtbl.add env (DVar x) (DVal ((vx, t), alignOpt))
	) varList;
	let boxList = collect_box_body b in
	List.iter (fun (i, tau) ->
		let alignOpt = genAlign env tau in
		let v = build_alloca (genAllocaType env tau) "_boxT" cont.builder in
		Option.iter (fun align -> set_alignment align v) alignOpt;
		Hashtbl.add env (DBox i) (DVal ((v, ptrType), alignOpt))
	) boxList

let genStructTD (cont: llvm_cont) (env: dusk_env) (f: string) (fl: (string * g_type) list): unit =
	let tau_l = List.map (fun (_, tau) -> genType tau) fl in
	(*let size = size_of_type cont (struct_type context (Array.of_list tau_l)) in*)
		(* filter types to only pointer types *)
	let ti_l = List.mapi (fun i tau -> (i, tau)) (List.map snd fl) in
	let tp_list = List.filter (fun (_, tau) -> isHeapType env tau) ti_l in
		(* if no heap pointers, use a null pointer *)
	if List.length tp_list = 0 then Hashtbl.add env (DTName f) (DTDef (StructTD_C(tau_l, const_null ptrType))) else (
			(* obtain offsets of each pointer value *)
		let offset_list = List.map (fun (i, _) -> DataLayout.offset_of_element (structType tau_l) i cont.data_layout) tp_list in
		let offsets_const = const_array iType (Array.of_list (List.map (fun i -> const_int iType (Int64.to_int i)) offset_list)) in
		let offsets_global = define_global ("tc_offs_" ^ f) offsets_const cont.llmod in
			(* create type information global *)
		let tc_inner = [const_int iType (List.length tp_list); offsets_global] in
		let tc_global = define_global ("tc_" ^ f) (const_struct context (Array.of_list tc_inner)) cont.llmod in
		Hashtbl.add env (DTName f) (DTDef (StructTD_C(tau_l, tc_global)))
	)

let genDec (cont: llvm_cont) (env: dusk_env) (f: string) (d: gen_dec): unit = match d with
	FunDecC (MethodC(pl, tau_r, b)) ->
		let fType = genFunType pl tau_r in
		let fVal = declare_function f fType cont.llmod in
		let block = append_block context "entry" fVal in
		position_at_end block (cont.builder);
		Hashtbl.add env (DVar f) (DFunVal(fVal, fType));
		let env' = genParamList cont env pl fVal in
		genPreAlloc cont env' b;
		ignore (genBody cont env' (1, fVal) b)
	| TDefDecC (StructTD fl) -> genStructTD cont env f fl
	| TDefDecC _ -> failwith "UNIMPLEMENTED: gen_exp.ml - type definition"
	| GlobalDecC(c, e) ->
		let (v, t) = genConstExp cont env e in
		let cVal = define_global f v cont.llmod in
		(if c then Llvm.set_global_constant true cVal else ());
		Hashtbl.add env (DVar f) (DGlobal (cVal, t))

let genDecList (cont: llvm_cont) (env: dusk_env) (dl: (string * gen_dec) list): unit =
	let _ = List.map (fun (f, d) -> genDec cont env f d) dl in ()

	(*
		external generation
	*)

let genGC (cont: llvm_cont): unit =
	let new_arr_type = function_type ptrType (Array.of_list [iType; iType; iType; iType]) in
	let new_arr = declare_function "gc_alloc_array" new_arr_type cont.llmod in
	let alloc_type = function_type ptrType (Array.of_list [iType; ptrType]) in
	let gc_alloc = declare_function "gc_alloc" alloc_type cont.llmod in
	let new_root_type = function_type voidType (Array.of_list [ptrType]) in
	let gc_new_root = declare_function "gc_new_root" new_root_type cont.llmod in
	let collect_type = function_type voidType (Array.of_list []) in
	let gc_collect = declare_function "gc_collect" collect_type cont.llmod in
	cont.gc := {
		new_array = (new_arr, new_arr_type);
		gc_alloc = (gc_alloc, alloc_type);
		gc_new_root = (gc_new_root, new_root_type);
		gc_collect = (gc_collect, collect_type);
	}

let rec genEnum (cont: llvm_cont) (env: dusk_env) (i: int) (cl: (canon_tag enum_case) list): unit = match cl with
	[] -> ()
	| (name, _, ext_o) :: ct -> let v = (match ext_o with
			None ->	DEnum i 
			| Some ext -> DGlobal (declare_global i8Type ext cont.llmod, i8Type)
		) in
		Hashtbl.add env (DCtor name) v;
		genEnum cont env (i + 1) ct

let genExternals (cont: llvm_cont) (env: dusk_env) (symList: g_virt_bind list): unit =
	let simpResList = ref [] in
	let simpPtrMap = Hashtbl.create 50 in
	let compResList = ref [] in
		(*
			generate external enum/function handles,
			collect resource handles
		*)
	List.iter (fun (_, f, vd) -> match vd with
		SymVD (ExternalSym vl, (tau_pl, tau_r)) ->
			let tau_plx = List.mapi (fun i tau_p -> if List.mem i vl then ptrType else genType tau_p) tau_pl in
			let fType = function_type (genType tau_r) (Array.of_list tau_plx) in
			let v = declare_function f fType cont.llmod in
			Hashtbl.add env (DVar f) (DFunVal(v, fType))
		| SymVD _ -> ()
		| TDefVD (EnumTD cl) ->
			let zero_size = size_of_type cont i8Type in
			let max_size = List.fold_left max zero_size (List.map (fun (_, tau_l, _) ->
				size_of_type cont (virtTagTupleType tau_l)
			) cl) in
			let max_align = List.fold_left max zero_size (List.map (fun (_, tau_l, _) ->
				align_of_type cont (virtTagTupleType tau_l)
			) cl) in
			(*let padding = (max_align - (max_size mod max_align)) mod max_align in*)
			Hashtbl.add env (DTName f) (DTDef (OpaqueTD_C(max_size, max_align)));
			genEnum cont env 0 cl
		| TDefVD (StructTD fl) -> genStructTD cont env f fl
		| ResVD(r, _) ->
			let ptr = define_global f (const_null ptrType) cont.llmod in
			Hashtbl.add env (DVar f) (DVal((ptr, ptrType), None)); (match r with
				SimpRes(ext, x, url) ->
					simpResList := (ext, url, ptr) :: !simpResList;
					Hashtbl.add simpPtrMap x ptr
				| CompRes(ext, xargs, args) ->
					compResList := (ext, xargs, args, ptr) :: !compResList
			)
	) symList;
		(* build URL + ptr list for simple resources *)
	let urlLitList = List.mapi (fun i (_, url, _) -> 
		let strVal = const_stringz context url in
		let g = define_global ("url_" ^ (string_of_int i)) strVal (cont.llmod) in
		set_global_constant true g;
		set_linkage Linkage.Private g; g
	) !simpResList in
	let urlArrVal = const_array ptrType (Array.of_list urlLitList) in
	let g = define_global "res_url_list" urlArrVal (cont.llmod) in
		(* - storage ptrs + total *)
	let ptrArrVal = const_array ptrType (Array.of_list (List.map (fun (_, _, ptr) -> ptr) !simpResList)) in
	let g_p = define_global "res_ptr_list" ptrArrVal (cont.llmod) in
	let g_n = define_global "res_total" (const_int iType (List.length !simpResList)) (cont.llmod) in
	set_global_constant true g; set_global_constant true g_p; set_global_constant true g_n;
		(* build argument + ptr list for composite resources *)
	let argsList = List.mapi (fun i (_, xargs, args, _) ->
		let xargList = List.map (fun x -> match Hashtbl.find_opt simpPtrMap x with
			Some ptr -> ptr
			| None -> failwith ("TO_ERR: gen_exp.ml - Composite resource using unknown source resource `" ^ x ^ "`")
		) xargs in
		let argList = List.map (fun i -> const_int iType i) args in
		let argWrapPtr = const_array iType (Array.of_list argList) in
		let g_ip = define_global ("cr_iargs" ^ (string_of_int i)) argWrapPtr (cont.llmod) in
		set_global_constant true g_ip;
		define_global ("cr_arg" ^ (string_of_int i)) (const_array ptrType (Array.of_list (g_ip :: xargList))) (cont.llmod)
	) !compResList in
	let argsVal = const_array ptrType (Array.of_list argsList) in
	let gc = define_global "comp_res_arg_list" argsVal (cont.llmod) in
		(* - storage ptrs + total *)
	let ptrArrVal = const_array ptrType (Array.of_list (List.map (fun (_, _, _, ptr) -> ptr) !compResList)) in
	let gc_p = define_global "comp_res_ptr_list" ptrArrVal (cont.llmod) in
	let gc_n = define_global "comp_res_total" (const_int iType (List.length !compResList)) (cont.llmod) in
	set_global_constant true gc; set_global_constant true gc_p; set_global_constant true gc_n

	(*
		code generation hook
		- pre-initializes context data structures so LLVM content doesn't end up as a dependency outside the library
	*)

let printAllTargets (): unit =
	let targetList = Target.all () in
	ignore (List.map (fun t -> print_string ((Target.name t) ^ " -- " ^ (Target.description t) ^ "\n")) targetList)

let genTarget (targetArg: string option) (optimizeFlag: bool): (llvm_cont * TargetMachine.t) =
	let newCont = Gen_cont.newLCont () in
		(* get target *)
	let target = (match targetArg with None -> Target.default_triple () | Some x -> x) in
	let ttx = Target.by_triple target in
	(*print_endline ("target: " ^ target);*)
	set_target_triple target newCont.llmod;
	(*printAllTargets ();*)
	(*let target = "x86-64" in
	let ttx = (match Target.by_name target with
		None -> failwith "Could not find target for specified backend."
		| Some t -> t
	) in*)
	(*let tm = print_endline ("target: " ^ target);*)
		(* create target machine *)
	let level = if optimizeFlag then CodeGenOptLevel.Aggressive else CodeGenOptLevel.None in
	let tm = TargetMachine.create ~triple:target ~cpu:"generic" ~features:"" ~level:level
		~reloc_mode:RelocMode.Default ~code_model:CodeModel.Default ttx in
		(* get data layout from target machine *)
	let layout = TargetMachine.data_layout tm in
	(*print_endline ("data layout: " ^ (DataLayout.as_string layout));*)
	let cont = { newCont with data_layout = layout } in
	set_data_layout (DataLayout.as_string layout) (cont.llmod); (cont, tm)

let genFinalize (cont: llvm_cont) (tm: TargetMachine.t) (fname: string): unit =
	let oc = open_out "log.ll" in
	output_string oc ("\n" ^ (string_of_llmodule cont.llmod));
	close_out oc; assert_valid_module cont.llmod;
	TargetMachine.emit_to_file cont.llmod (CodeGenFileType.ObjectFile) (fname ^ ".o") tm;
	TargetMachine.emit_to_file cont.llmod (CodeGenFileType.AssemblyFile) (fname ^ ".xx") tm

let genProgramHook (targetArg: string option) (fname: string) (optimizeFlag: bool)
	(symList: g_virt_bind list) (dl: (string * gen_dec) list): unit =
	let (cont, tm) = genTarget targetArg optimizeFlag in
	let env = Hashtbl.create 50 in
	genGC cont;
	genExternals cont env symList;
	genDecList cont env dl;
	genFinalize cont tm fname;;