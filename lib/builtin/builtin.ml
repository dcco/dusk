open Commons.Tree_map
open Parser.Dusk_type

	(*
		virtual declarations: special declarations to "compile" builtin functions / types
		* symbols: < builtin functions, and what they should compile to >
		-- binary expression ASM
		-- internal bindings
			< miscellaneous special function that executes at compile-time >
		-- external bindings
			< integer list describing "virtual" arguments - which arguments must be wrapped in ptrs > 
		-- user-defined: < not a builtin function >
		* type definition: < builtin types w/ a concrete definition (mostly ADTs) >
		* resource: < builtin resources defined from ROM/layout >
		-- simple resource
			< file extension, handle (for composites), URL >
		-- composite resource
			< composite type, string arguments, integer arguments >
	*)

type sym =
	UnaryASMSym of string
	| BinaryASMSym of string
	| InternalSym of string
	| ExternalSym of int list
	| UserDefSym

type resource_def =
	SimpRes of string * string * string
	| CompRes of string * string list * int list

type 'm virt_dec =
	SymVD of sym * 'm fun_type
	| TDefVD of 'm raw_tdef
		(* url *)
	| ResVD of resource_def * 'm raw_type

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
	("neg", UnaryASMSym "ineg", [intTy], intTy);
	("add", BinaryASMSym "iadd", [intTy; intTy], intTy);
	("sub", BinaryASMSym "isub", [intTy; intTy], intTy);
	("mul", BinaryASMSym "imul", [intTy; intTy], intTy);
	("div", BinaryASMSym "idiv", [intTy; intTy], intTy);
	("mod", BinaryASMSym "imod", [intTy; intTy], intTy);

	("neg", UnaryASMSym "fneg", [floatTy], floatTy);
	("add", BinaryASMSym "fadd", [floatTy; floatTy], floatTy);
	("sub", BinaryASMSym "fsub", [floatTy; floatTy], floatTy);
	("mul", BinaryASMSym "fmul", [floatTy; floatTy], floatTy);
	("flDiv", BinaryASMSym "fdiv", [floatTy; floatTy], floatTy);

	("eq", BinaryASMSym "ieq", [intTy; intTy], boolTy);
	("neq", BinaryASMSym "ineq", [intTy; intTy], boolTy);
	("leq", BinaryASMSym "ileq", [intTy; intTy], boolTy);
	("lt", BinaryASMSym "ilt", [intTy; intTy], boolTy);
	("geq", BinaryASMSym "igeq", [intTy; intTy], boolTy);
	("gt", BinaryASMSym "igt", [intTy; intTy], boolTy);

	("eq", BinaryASMSym "feq", [floatTy; floatTy], boolTy);
	("neq", BinaryASMSym "fneq", [floatTy; floatTy], boolTy);
	("leq", BinaryASMSym "fleq", [floatTy; floatTy], boolTy);
	("lt", BinaryASMSym "flt", [floatTy; floatTy], boolTy);
	("geq", BinaryASMSym "fgeq", [floatTy; floatTy], boolTy);
	("gt", BinaryASMSym "fgt", [floatTy; floatTy], boolTy);

	("not", UnaryASMSym "bnot", [boolTy], boolTy);
	("and", BinaryASMSym "band", [boolTy; boolTy], boolTy);
	("or", BinaryASMSym "bor", [boolTy; boolTy], boolTy);

	("add", BinaryASMSym "i64add", [longTy; longTy], longTy);
	("sub", BinaryASMSym "i64sub", [longTy; longTy], longTy);
	("mul", BinaryASMSym "i64mul", [longTy; longTy], longTy);
	("div", BinaryASMSym "i64div", [longTy; longTy], longTy);

	("add", ExternalSym [], [stringTy; stringTy], stringTy);
	("toString", ExternalSym [], [intTy], stringTy);
	("toString", ExternalSym [], [floatTy], stringTy);

	("toInt", UnaryASMSym "ftoi", [floatTy], intTy);
	("toInt", UnaryASMSym "i64toi", [longTy], intTy);
	("toFloat", UnaryASMSym "itof", [intTy], floatTy);
	("toLong", UnaryASMSym "itoi64", [intTy], longTy);
	("floor", ExternalSym [], [floatTy], floatTy);

	("expo", ExternalSym [], [floatTy; floatTy], floatTy);
	("sqrt", ExternalSym [], [floatTy], floatTy);
	("abs", ExternalSym [], [floatTy], floatTy);

	("toRadians", ExternalSym [], [intTy], floatTy);

	("measure", ExternalSym [], [stringTy], intTy);

	("cLoad", InternalSym "cLoad", [stringTy], stringTy)
]

let prngTy = builtinTy "PRNG"

let osList = [
	("print", ExternalSym [], [stringTy], unitTy);

	("randomInt", ExternalSym [], [intTy], intTy);
	("randomFloat", ExternalSym [], [], floatTy);

	("newPRNG", ExternalSym [], [intTy], prngTy);
	("randomInt", ExternalSym [], [prngTy; intTy], intTy);
	("randomFloat", ExternalSym [], [prngTy], floatTy);
	
	("time", ExternalSym [], [], longTy);
]

let inputList = [
	("update", ExternalSym [], [], unitTy);
	("keyDown", ExternalSym [], [keyTy], boolTy);
	("keyPress", ExternalSym [], [keyTy], boolTy)
]

let mat4Ty = builtinTy "Mat4"
let shaderTy = builtinTy "Shader"
let renderDataTy = builtinTy "RenderData"
let imageTy = builtinTy "Image"
let spriteTy = builtinTy "Sprite"

let sulfurList = [
	("refresh", ExternalSym [], [], unitTy);
	("draw", ExternalSym [], [namedTy "Glyph"], unitTy);
	("draw", ExternalSym [], [namedTy "Glyph3d"], unitTy);

	("nullShader", InternalSym "null", [], shaderTy);
	("newShader", ExternalSym [], [stringTy; stringTy; ArrayTy(1, intTy);
		ArrayTy(1, TupleTy [stringTy; namedTy "GLType"; intTy])
	], shaderTy);
	("setUniform", ExternalSym [], [shaderTy; intTy; namedTy "GLVal"], unitTy);
	("render", ExternalSym [], [shaderTy; renderDataTy], unitTy);

	("alloc", ExternalSym [], [renderDataTy; intTy], unitTy);
	("get", ExternalSym [], [renderDataTy; intTy], namedTy "GLVal");
	("set", ExternalSym [], [renderDataTy; intTy; namedTy "GLVal"], unitTy);

	("nullMat4", InternalSym "null", [], mat4Ty);
	("newMat4", ExternalSym [], [], mat4Ty);
	("idMat4", ExternalSym [], [mat4Ty], unitTy);
	("translate", ExternalSym [], [mat4Ty; floatTy; floatTy; floatTy], unitTy);
	("rotateX", ExternalSym [], [mat4Ty; floatTy], unitTy);
]

let sulfurTypes = [
	(QT None, "Glyph", TDefVD (EnumTD [
		("GNop", [], Some "C_NOP");
		("GBox", [intTy; intTy; intTy; intTy; intTy], Some "C_BOX");
		("GSprite", [intTy; intTy; spriteTy; intTy], Some "C_SPRITE");
		("GText", [spriteTy; stringTy], Some "C_TEXT")
	]));
	(QT None, "Glyph3d", TDefVD (EnumTD [
		("G3Nop", [], Some "C3_NOP");
		("G3QuadX", [floatTy; floatTy; floatTy; spriteTy; intTy], Some "C3_QX");
		("G3QuadY", [floatTy; floatTy; floatTy; spriteTy; intTy], Some "C3_QY");
		("G3QuadZ", [floatTy; floatTy; floatTy; spriteTy; intTy], Some "C3_QZ");
	]));
	(QT None, "GLType", TDefVD (EnumTD [
		("GLFloat", [], Some "C_GL_FLOAT");
		("GLMat4", [], Some "C_GL_MAT4");
	]));
	(QT None, "GLVal", TDefVD (EnumTD [
		("GLFloatV", [floatTy], Some "C_GL_FLOAT");
		("GLMat4V", [mat4Ty], Some "C_GL_MAT4");
	]));
]

	(*
		builtin tree map
	*)

let builtinTreeMap (): (m_virt_bind list) tree_map =
	let m1 = single_tree ["builtin"] (toVirtList builtinList) in
	let m2 = add_tree m1 ["Sys"; "Os"] (toVirtList osList) in
	let m3 = add_tree m2 ["Sys"; "Input"] (toVirtList inputList) in
	add_tree m3 ["Sys"; "Sulfur"] (sulfurTypes @ (toVirtList sulfurList))

(*
let builtinQualList (): (string list * m_virt_bind) list =
	List.concat (List.map (fun (path, vdl) ->
		List.map (fun vd -> (path, vd)) vdl
	) (flatten_tree (builtinTreeMap ())))
*)

type prim_flag = PF | NPF

let extractSymbols (symList: 'm virt_bind list): (prim_flag * string) list =
	List.concat (List.map (fun (_, f, vd) -> match vd with
		TDefVD (EnumTD cl) -> (PF, f) :: (List.map (fun (c, _, _) -> (PF, c)) cl)
		| _ -> [(NPF, f)]
	) symList)