open Llvm

open Parser.Dusk_ast
open Fin_type
open Fin_ast
open Gen_cont
open Gen_type

	(* expression generation *)

let genConst (c: const): dusk_val = match c with
	IConst i -> (const_int iType i, iType)
	| FConst f -> (const_float fType f, fType)
	| SConst _ -> failwith "BUG: gen_exp.ml - Raw string literal in generation phase."
	| BConst b -> (const_int bType (if b then 1 else 0), bType)

let rec genExp (cont: llvm_cont) (env: dusk_env) (e: gen_exp): dusk_val = let bx = builder cont in match e with
	ConstExpC c -> genConst c
	| LitExpC i -> (match Hashtbl.find env (DLitId i) with
		DVal (v, t) -> (v, t)
		| _ -> failwith ("BUG: gen_exp.ml - String literal incorrectly resolved in generation phase.")
	)
	| VarExpC x -> (match Hashtbl.find_opt env (DVar x) with
		Some (DFunVal (v, t)) -> (v, t)
		| Some (DVal (v, t)) -> (build_load t v x bx, t)
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

	(* statement generation *)

type blockInfo = int * llvalue

let addBlock ((i, fVal): blockInfo) (prefix: string): (llbasicblock * blockInfo) =
	(append_block context (prefix ^ "_" ^ (string_of_int i)) fVal, (i + 1, fVal))

let rec genStmt (cont: llvm_cont) (env: dusk_env) (b: blockInfo) (s: gen_stmt): blockInfo = match s with
	EvalStmtC e -> let _ = genExp cont env e in b
	| AssignStmtC(x, e) ->
		let (ve, _) = genExp cont env e in
		(match Hashtbl.find env (DVar x) with
			DVal (vx, _) ->
				let _ = build_store ve vx (builder cont) in b
			| _ -> failwith "BUG: gen_exp.ml - Unexpected assignment to function variable."
		)
	| ReturnStmtC rv -> let _ = (match rv with
		None -> build_ret_void (builder cont)
		| Some e ->
			let (ve, _) = genExp cont env e in build_ret ve (builder cont)) in b
	| VarStmtC(x, e) ->
		let (ve, t) = genExp cont env e in
		let vx = build_alloca t x (builder cont) in
			Hashtbl.add env (DVar x) (DVal (vx, t));
			let _ = build_store ve vx (builder cont) in b
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
			let (vp, t) = (Array.get va i, genType env tau) in
			let vx = build_alloca t ("_" ^ x) (builder cont) in
			let _ = build_store vp vx (builder cont) in
			Hashtbl.add env' (DVar x) (DVal((vx, t))); gpl_rec pt (i + 1)
	in gpl_rec pl 0; env'

let genDec (cont: llvm_cont) (env: dusk_env) (f: string) (FunDecC(pl, tau_r, b): gen_dec): unit =
	let fType = genFunType env pl tau_r in
	let fVal = declare_function f fType (llmod cont) in
	let block = append_block context "entry" fVal in
	position_at_end block (builder cont);
	Hashtbl.add env (DVar f) (DFunVal(fVal, fType));
	let env' = genParamList cont env pl fVal in
	ignore (genBody cont env' (1, fVal) b)

let genDecList (cont: llvm_cont) (env: dusk_env) (dl: (string * gen_dec) list): unit =
	let _ = List.map (fun (f, d) -> genDec cont env f d) dl in ()