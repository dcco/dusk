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
	let strVal = const_string context s in
	let strRef = define_global ("_s" ^ (string_of_int i)) strVal (llmod cont) in
		(* create (pointer to) struct containing string + meta data *)
	let structVal = const_struct context
		[| const_int i8Type 0; const_int iType (String.length s); const_null ptrType; const_null ptrType; strRef |] in
	let structRef = define_global ("_sz" ^ (string_of_int i)) structVal (llmod cont) in
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

		(* failwith "BUG: gen_exp.ml - Raw string literal in generation phase." *)

let rec genExp (cont: llvm_cont) (env: dusk_env) (e: gen_exp): dusk_val = let bx = builder cont in match e with
	ConstExpC c -> genConst cont env c
	(*| LitExpC i -> (match Hashtbl.find env (DLitId i) with
		DVal (v, t) -> (v, t)
		| _ -> failwith ("BUG: gen_exp.ml - String literal incorrectly resolved in generation phase.")
	)*)
	| VarExpC x -> (match Hashtbl.find_opt env (DVar x) with
		Some (DFunVal (v, t)) -> (v, t)
		| Some (DVal ((v, t), alignOpt)) ->
			let vx = build_load t v x bx in
			Option.iter (fun align -> set_alignment align vx) alignOpt; (vx, t)
		| Some _ -> failwith ("BUG: gen_exp.ml - Variable \"" ^ x ^ "\" resolved to non-value.")
		| None -> failwith ("BUG: gen_exp.ml - Unexpected variable \"" ^ x ^ "\" encountered in generation phase.")
	)
	| BinExpC(xOp, e1, e2) ->
		let (v1, _) = genExp cont env e1 in
		let (v2, _) = genExp cont env e2 in (match xOp with
			"iadd" -> (build_add v1 v2 "_addT" bx, iType)
			| "isub" -> (build_sub v1 v2 "_subT" bx, iType)
			| "imul" -> (build_mul v1 v2 "_mulT" bx, iType)
			| "idiv" -> (build_sdiv v1 v2 "_divT" bx, iType)
			| "fadd" -> (build_fadd v1 v2 "_addT" bx, fType)
			| "fsub" -> (build_fsub v1 v2 "_subT" bx, fType)
			| "fmul" -> (build_fmul v1 v2 "_mulT" bx, fType)
			| "ieq" -> (build_icmp Icmp.Eq v1 v2 "_cmpT" bx, bType)
			| "ineq" -> (build_icmp Icmp.Ne v1 v2 "_cmpT" bx, bType)
			| "ileq" -> (build_icmp Icmp.Sle v1 v2 "_cmpT" bx, bType)
			| "ilt" -> (build_icmp Icmp.Slt v1 v2 "_cmpT" bx, bType)
			| "igeq" -> (build_icmp Icmp.Sge v1 v2 "_cmpT" bx, bType)
			| "igt" -> (build_icmp Icmp.Sgt v1 v2 "_cmpT" bx, bType)
			| _ -> failwith ("BUG: gen_exp.ml - Unexpected operator \"" ^ xOp ^ "\" encountered in generation phase.")
		)
	| CallExpC(ef, el, tau_r) ->
		let (vf, tf) = genExp cont env ef in
		let vl = List.map (fun e -> fst (genExp cont env e)) el in
		(build_call tf vf (Array.of_list vl) "" (builder cont), genType env tau_r)
	| BoxExpC(i, e, _) ->
		let (ve, _) = genExp cont env e in (match Hashtbl.find_opt env (DBox i) with
			Some (DVal ((vb, tb), alignOpt)) ->
				let vs = build_store ve vb bx in
				Option.iter (fun align -> set_alignment align vs) alignOpt; (vb, tb)
			| _ -> failwith "BUG: gen_exp.ml - Ungenerated box encountered in generation phase."
		)
	| TupleExpC el ->
		 	(* compile the sub-expressions *)
		let res_l = List.map (fun e -> genExp cont env e) el in
		let tau_l = List.map snd res_l in
			(* initialize struct + fields *)
		let tau_enum = structType tau_l in
		let (sVal, _) = List.fold_left (fun (sVal, i) (v, _) ->
			let sVal' = build_insertvalue sVal v i "_stT" bx in (sVal', i + 1)
		) (undef tau_enum, 0) res_l in
		(sVal, tau_enum)
	| TagTupleExpC(tag, el) ->
			(* find the tag literal *)
		let tagLit = (match Hashtbl.find_opt env (DCtor tag) with
			Some (DEnum i) -> const_int i8Type i
			| Some (DGlobal p) -> build_load i8Type p "_G" bx
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
		(sVal, tau_enum)
	| TupleIndexExpC(ep, i, tau_v) ->
		let (vp, _) = genExp cont env ep in
		(*let sVal = build_load vp bx "_structT" in*)
		let elem = build_extractvalue vp i "_elemT" bx in
		(elem, genType env tau_v)
	| NewStructExpC el ->
		


	(* statement generation *)

type blockInfo = int * llvalue

let addBlock ((i, fVal): blockInfo) (prefix: string): (llbasicblock * blockInfo) =
	(append_block context (prefix ^ "_" ^ (string_of_int i)) fVal, (i + 1, fVal))

let genAssign (cont: llvm_cont) (env: dusk_env) (x: string) (e: gen_exp): unit =
	let (ve, _) = genExp cont env e in
	(match Hashtbl.find env (DVar x) with
		DVal ((vx, _), alignOpt) ->
			let vs = build_store ve vx (builder cont) in
			Option.iter (fun align -> set_alignment align vs) alignOpt
		| _ -> failwith "BUG: gen_exp.ml - Unexpected assignment to function variable."
	)

let rec genStmt (cont: llvm_cont) (env: dusk_env) (b: blockInfo) (s: gen_stmt): blockInfo = match s with
	EvalStmtC e -> let _ = genExp cont env e in b
	| AssignStmtC(x, e) -> genAssign cont env x e; b
	| VarStmtC(x, e, _) -> genAssign cont env x e; b
	| ReturnStmtC rv -> let _ = (match rv with
		None -> build_ret_void (builder cont)
		| Some e ->
			let (ve, _) = genExp cont env e in build_ret ve (builder cont)) in b
	| IfStmtC(ec, body, term1, elseBody, term2) ->
		let (vc, _) = genExp cont env ec in
			(* create branch statement *)
		let (block1, b1) = addBlock b "body" in
		let (block2, b2) = addBlock b1 "else_body" in
		let _ = build_cond_br vc block1 block2 (builder cont) in
			(* stub out final block when necessary *)
		let (blockF, bx) = if not (term1 && term2) then addBlock b2 "join" else (block2, b2) in
			(* generate block 1 + 2, join when relevant *)
		position_at_end block1 (builder cont);
		let bx' = genBody cont env bx body in
		if not term1 then ignore (build_br blockF (builder cont)) else ();
		position_at_end block2 (builder cont);
		let bf = genBody cont env bx' elseBody in
		if not term2 then ignore (build_br blockF (builder cont)) else ();
			(* re-position at final join *)
		position_at_end blockF (builder cont); bf
	| WhileStmtC(ec, body) ->
			(* create loop starting block *)
		let (block1, b1) = addBlock b "cond" in
		ignore (build_br block1 (builder cont));
		position_at_end block1 (builder cont);
		let (vc, _) = genExp cont env ec in
			(* create branch statement *)
		let (block2, b2) = addBlock b1 "body" in 
		let (blockF, b3) = addBlock b2 "end" in 
		let _ = build_cond_br vc block2 blockF (builder cont) in
			(* generate main block, branch to start *)
		position_at_end block2 (builder cont);
		let bf = genBody cont env b3 body in
		ignore (build_br block1 (builder cont));
		position_at_end blockF (builder cont); bf

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
			let (vp, t, alignOpt) = (Array.get va i, genType env tau, genAlign env tau) in
			let vx = build_alloca t ("_" ^ x) (builder cont) in
			let vs = build_store vp vx (builder cont) in
			Option.iter (fun align -> set_alignment align vx; set_alignment align vs) alignOpt;
			Hashtbl.add env' (DVar x) (DVal ((vx, t), alignOpt)); gpl_rec pt (i + 1)
	in gpl_rec pl 0; env'

let genPreAlloc (cont: llvm_cont) (env: dusk_env) (b: gen_stmt list): unit =
	let varList = collect_var_body b in
	List.iter (fun (x, tau) ->
		let t = genType env tau in
		let alignOpt = genAlign env tau in
		let vx = build_alloca t ("_" ^ x) (builder cont) in
		Option.iter (fun align -> set_alignment align vx) alignOpt;
		Hashtbl.add env (DVar x) (DVal ((vx, t), alignOpt))
	) varList;
	let boxList = collect_box_body b in
	List.iter (fun (i, _, tau) ->
		let alignOpt = genAlign env tau in
		let v = build_alloca (genType env tau) "_boxT" (builder cont) in
		Option.iter (fun align -> set_alignment align v) alignOpt;
		Hashtbl.add env (DBox i) (DVal ((v, ptrType), alignOpt))
	) boxList

let genDec (cont: llvm_cont) (env: dusk_env) (f: string) (FunDecC (MethodC(pl, tau_r, b)): gen_dec): unit =
	let fType = genFunType env pl tau_r in
	let fVal = declare_function f fType (llmod cont) in
	let block = append_block context "entry" fVal in
	position_at_end block (builder cont);
	Hashtbl.add env (DVar f) (DFunVal(fVal, fType));
	let env' = genParamList cont env pl fVal in
	genPreAlloc cont env' b;
	ignore (genBody cont env' (1, fVal) b)

let genDecList (cont: llvm_cont) (env: dusk_env) (dl: (string * gen_dec) list): unit =
	let _ = List.map (fun (f, d) -> genDec cont env f d) dl in ()

	(*
	*)

let rec genEnum (cont: llvm_cont) (env: dusk_env) (i: int) (cl: (canon_tag enum_case) list): unit = match cl with
	[] -> ()
	| (name, _, ext_o) :: ct -> let v = (match ext_o with
			None ->	DEnum i 
			| Some ext -> DGlobal (declare_global i8Type ext (llmod cont))
		) in
		Hashtbl.add env (DCtor name) v;
		genEnum cont env (i + 1) ct

let genExternals (cont: llvm_cont) (env: dusk_env) (symList: g_virt_bind list): unit =
	let simpResList = ref [] in
	let simpPtrMap = Hashtbl.create 50 in
	let compResList = ref [] in
	List.iter (fun (_, f, vd) -> match vd with
		SymVD (ExternalSym vl, (tau_pl, tau_r)) ->
			let tau_plx = List.mapi (fun i tau_p -> if List.mem i vl then ptrType else genType env tau_p) tau_pl in
			let fType = function_type (genType env tau_r) (Array.of_list tau_plx) in
			let v = declare_function f fType (llmod cont) in
			Hashtbl.add env (DVar f) (DFunVal(v, fType))
		| SymVD _ -> ()
		| TDefVD (EnumTD cl) ->
			let zero_size = size_of_type cont i8Type in
			let max_size = List.fold_left max zero_size (List.map (fun (_, tau_l, _) ->
				size_of_type cont (genTagTupleType env tau_l)
			) cl) in
			let max_align = List.fold_left max zero_size (List.map (fun (_, tau_l, _) ->
				align_of_type cont (genTagTupleType env tau_l)
			) cl) in
			(*let padding = (max_align - (max_size mod max_align)) mod max_align in*)
			Hashtbl.add env (DTName f) (DTDef (OpaqueTD_C(max_size, max_align)));
			genEnum cont env 0 cl
		| TDefVD (StructTD _) -> ()
		| ResVD(r, _) ->
			let ptr = define_global f (const_null ptrType) (llmod cont) in
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
		let g = define_global ("url_" ^ (string_of_int i)) strVal (llmod cont) in
		set_global_constant true g;
		set_linkage Linkage.Private g; g
	) !simpResList in
	let urlArrVal = const_array ptrType (Array.of_list urlLitList) in
	let g = define_global "res_url_list" urlArrVal (llmod cont) in
		(* - storage ptrs + total *)
	let ptrArrVal = const_array ptrType (Array.of_list (List.map (fun (_, _, ptr) -> ptr) !simpResList)) in
	let g_p = define_global "res_ptr_list" ptrArrVal (llmod cont) in
	let g_n = define_global "res_total" (const_int iType (List.length !simpResList)) (llmod cont) in
	set_global_constant true g; set_global_constant true g_p; set_global_constant true g_n;
		(* build argument + ptr list for composite resources *)
	let argsList = List.mapi (fun i (_, xargs, args, _) ->
		let xargList = List.map (fun x -> match Hashtbl.find_opt simpPtrMap x with
			Some ptr -> ptr
			| None -> failwith ("TO_ERR: gen_exp.ml - Composite resource using unknown source resource `" ^ x ^ "`")
		) xargs in
		let argList = List.map (fun i -> const_int iType i) args in
		let argWrapPtr = const_array iType (Array.of_list argList) in
		let g_ip = define_global ("cr_iargs" ^ (string_of_int i)) argWrapPtr (llmod cont) in
		set_global_constant true g_ip;
		define_global ("cr_arg" ^ (string_of_int i)) (const_array ptrType (Array.of_list (g_ip :: xargList))) (llmod cont)
	) !compResList in
	let argsVal = const_array ptrType (Array.of_list argsList) in
	let gc = define_global "comp_res_arg_list" argsVal (llmod cont) in
		(* - storage ptrs + total *)
	let ptrArrVal = const_array ptrType (Array.of_list (List.map (fun (_, _, _, ptr) -> ptr) !compResList)) in
	let gc_p = define_global "comp_res_ptr_list" ptrArrVal (llmod cont) in
	let gc_n = define_global "comp_res_total" (const_int iType (List.length !compResList)) (llmod cont) in
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
	set_target_triple target (llmod newCont);
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
	let cont = setLayoutCont newCont layout in
	set_data_layout (DataLayout.as_string layout) (llmod cont); (cont, tm)

let genFinalize (cont: llvm_cont) (tm: TargetMachine.t) (fname: string): unit =
	print_string ("\n" ^ (string_of_llmodule (llmod cont))); assert_valid_module (llmod cont);
	TargetMachine.emit_to_file (llmod cont) (CodeGenFileType.ObjectFile) (fname ^ ".o") tm;
	TargetMachine.emit_to_file (llmod cont) (CodeGenFileType.AssemblyFile) (fname ^ ".xx") tm

let genProgramHook (targetArg: string option) (fname: string) (optimizeFlag: bool)
	(symList: g_virt_bind list) (dl: (string * gen_dec) list): unit =
	let (cont, tm) = genTarget targetArg optimizeFlag in
	let env = Hashtbl.create 50 in
	genExternals cont env symList;
	genDecList cont env dl;
	genFinalize cont tm fname;;