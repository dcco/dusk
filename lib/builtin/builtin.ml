open Commons.Tree_map
open Parser.Dusk_type

	(*
		virtual declarations: special declarations to "compile" builtin functions / types
		-- binary expression ASM
		-- external bindings
		>>> list of integers indicating "virtual" arguments, which arguments must be wrapped in ptrs 

	*)

type sym =
	BinaryASMSym of string
	| ExternalSym of int list
	| UserDefSym

type 'm virt_dec =
	SymVD of sym * 'm fun_type
	| TDefVD of 'm raw_tdef

type 'm virt_bind = 'm * string * 'm virt_dec

type m_virt_bind = qual_tag virt_bind

	(*
		builtins
	*)

let toVirtList (rawList: (string * sym * m_type list * m_type) list): m_virt_bind list =
	List.map (fun (x, v, tau_pl, tau_r) -> (QT None, x, SymVD(v, (tau_pl, tau_r)))) rawList

(*
let completeTdefList (vl: m_virt_bind list): virt_bind list =
	List.fold_right (fun (f, v) nl -> match v with
		TDefVD (EnumTD cl) -> (List.map (fun (c, _, _) -> (c, CtorVD f)) cl) @ nl
		| _ -> nl
	) vl vl
*)
let builtinList =  [
	("add", BinaryASMSym "iadd", [intTy; intTy], intTy);
	("sub", BinaryASMSym "isub", [intTy; intTy], intTy);
	("mul", BinaryASMSym "imul", [intTy; intTy], intTy);
	("div", BinaryASMSym "idiv", [intTy; intTy], intTy);

	("add", BinaryASMSym "fadd", [floatTy; floatTy], floatTy);
	("sub", BinaryASMSym "fsub", [floatTy; floatTy], floatTy);
	("mul", BinaryASMSym "fmul", [floatTy; floatTy], floatTy);

	("eq", BinaryASMSym "ieq", [intTy; intTy], boolTy);
	("neq", BinaryASMSym "ineq", [intTy; intTy], boolTy);
	("leq", BinaryASMSym "ileq", [intTy; intTy], boolTy);
	("lt", BinaryASMSym "ilt", [intTy; intTy], boolTy);
	("geq", BinaryASMSym "igeq", [intTy; intTy], boolTy);
	("gt", BinaryASMSym "igt", [intTy; intTy], boolTy);

	("add", ExternalSym [], [stringTy; stringTy], stringTy);
	("toString", ExternalSym [], [intTy], stringTy);
	("toString", ExternalSym [], [floatTy], stringTy);
]

let osList = [
	("print", ExternalSym [], [stringTy], unitTy);

	("randomInt", ExternalSym [], [intTy], intTy);
	("randomFloat", ExternalSym [], [], floatTy)
]

let sulfurList = [
	("refresh", ExternalSym [], [], unitTy);
	("draw", ExternalSym [0], [namedTy "Glyph"], unitTy);
	("drawTX", ExternalSym [0], [TupleTy [intTy; intTy; intTy; intTy]], unitTy)
]

let sulfurTypes = [
	(QT None, "Glyph", TDefVD (EnumTD [
		("Nop", [], Some "C_NOP");
		("Box", [intTy; intTy; intTy; intTy], Some "C_BOX")
	]))
]

	(*
		builtin tree map
	*)

let builtinTreeMap (): (m_virt_bind list) tree_map =
	let m1 = single_tree ["builtin"] (toVirtList builtinList) in
	let m2 = add_tree m1 ["Sys"; "Os"] (toVirtList osList) in
	add_tree m2 ["Sys"; "Sulfur"] (sulfurTypes @ (toVirtList sulfurList))

let builtinQualList (): (string list * m_virt_bind) list =
	List.concat (List.map (fun (path, vdl) ->
		List.map (fun vd -> (path, vd)) vdl
	) (flatten_tree (builtinTreeMap ())))

type prim_flag = PF | NPF

let extractSymbols (symList: 'm virt_bind list): (prim_flag * string) list =
	List.concat (List.map (fun (_, f, vd) -> match vd with
		TDefVD (EnumTD cl) -> (PF, f) :: (List.map (fun (c, _, _) -> (PF, c)) cl)
		| _ -> [(NPF, f)]
	) symList)