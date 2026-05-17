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

type raw_bind =
	PrimBind of string
	| RawBind of string

let pb x = PrimBind x
let rb x = RawBind x

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

type ('b, 'm) virt_bind = 'b * 'm virt_dec

type m_virt_bind = (raw_bind, qual_name) virt_bind

	(*
		builtins
	*)

let toVirtList (rawList: (string * sym * m_type list * m_type) list): m_virt_bind list =
	List.map (fun (x, v, tau_pl, tau_r) -> (rb x, SymVD(v, (tau_pl, tau_r)))) rawList

(*
let completeTdefList (vl: m_virt_bind list): virt_bind list =
	List.fold_right (fun (f, v) nl -> match v with
		TDefVD (EnumTD cl) -> (List.map (fun (c, _, _) -> (c, CtorVD f)) cl) @ nl
		| _ -> nl
	) vl vl
*)
let builtinList = [
	("neg", UnaryASMSym "ineg", [intTy], intTy);
	("add", BinaryASMSym "iadd", [intTy; intTy], intTy);
	("sub", BinaryASMSym "isub", [intTy; intTy], intTy);
	("mul", BinaryASMSym "imul", [intTy; intTy], intTy);
	("div", BinaryASMSym "idiv", [intTy; intTy], intTy);
	("mod", BinaryASMSym "imod", [intTy; intTy], intTy);
	("flDiv", BinaryASMSym "ifdiv", [intTy; intTy], floatTy);

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

	("add", BinaryASMSym "ui64add", [uint64Ty; uint64Ty], uint64Ty);
	("sub", BinaryASMSym "ui64sub", [uint64Ty; uint64Ty], uint64Ty);
	("mul", BinaryASMSym "ui64mul", [uint64Ty; uint64Ty], uint64Ty);
	("div", BinaryASMSym "ui64div", [uint64Ty; uint64Ty], uint64Ty);

	("eq", BinaryASMSym "ieq", [uint32Ty; uint32Ty], boolTy);
	("neq", BinaryASMSym "ineq", [uint32Ty; uint32Ty], boolTy);

	("eq", BinaryASMSym "ieq", [uint8Ty; uint8Ty], boolTy);
	("neq", BinaryASMSym "ineq", [uint8Ty; uint8Ty], boolTy);

	("add", ExternalSym [], [stringTy; stringTy], stringTy);
	("toString", ExternalSym [], [intTy], stringTy);
	("toString", ExternalSym [], [floatTy], stringTy);

	("toInt", UnaryASMSym "ftoi", [floatTy], intTy);
	("toInt", UnaryASMSym "ui64toi", [uint64Ty], intTy);
	("toFloat", UnaryASMSym "itof", [intTy], floatTy);
	("toU64", UnaryASMSym "itoui64", [intTy], uint64Ty);
	("floor", ExternalSym [], [floatTy], intTy);
	("ceil", ExternalSym [], [floatTy], intTy);

	("expo", ExternalSym [], [floatTy; floatTy], floatTy);
	("sqrt", ExternalSym [], [floatTy], floatTy);
	("abs", ExternalSym [], [floatTy], floatTy);

	("toRadians", ExternalSym [], [intTy], floatTy);

	("measure", ExternalSym [], [stringTy], intTy);

	("color", ExternalSym [], [uint8Ty; uint8Ty; uint8Ty], uint32Ty);
	("rgb", ExternalSym [], [uint32Ty], uint32Ty);

	("remove", ExternalSym [], [ArrayTy(1, intTy); intTy], unitTy);

	("cLoad", InternalSym "cLoad", [stringTy], stringTy);
]

let prngTy = builtinTy "PRNG"

let osList = [
	("print", ExternalSym [], [stringTy], unitTy);

	("randomInt", ExternalSym [], [intTy], intTy);
	("randomFloat", ExternalSym [], [], floatTy);

	("newPRNG", ExternalSym [], [intTy], prngTy);
	("randomInt", ExternalSym [], [prngTy; intTy], intTy);
	("randomFloat", ExternalSym [], [prngTy], floatTy);
	
	("time", ExternalSym [], [], uint64Ty);
]

let inputList = [
	("inUpdate", ExternalSym [], [], unitTy);
	("keyDown", ExternalSym [], [keyTy], boolTy);
	("keyPress", ExternalSym [], [keyTy], boolTy)
]

let vec3Ty = TupleTy [floatTy; floatTy; floatTy]
let mat4Ty = builtinTy "Mat4"
let shaderTy = builtinTy "Shader"
let fboTy = builtinTy "FrameBuffer"
let renderDataTy = builtinTy "RenderData"
let imageTy = builtinTy "Image"
let spriteTy = builtinTy "Sprite"

let sulfurList = [
		(* main hooks *)
	("waitRom", ExternalSym [], [], unitTy);
	("refresh", ExternalSym [], [], unitTy);
	("draw", ExternalSym [], [namedTy "Glyph"], unitTy);
	("drawQuadX", ExternalSym [], [floatTy; floatTy; floatTy; spriteTy; intTy], unitTy);
	("drawQuadY", ExternalSym [], [floatTy; floatTy; floatTy; spriteTy; intTy], unitTy);
	("drawQuadZ", ExternalSym [], [floatTy; floatTy; floatTy; spriteTy; intTy], unitTy);
	("drawSprite", ExternalSym [], [floatTy; floatTy; floatTy; spriteTy; intTy; boolTy], unitTy);

		(* rom data *)
	("pixel", ExternalSym [], [imageTy; intTy; intTy], uint32Ty);

		(* shader / fbo setup *)
	("newShader", ExternalSym [], [stringTy; stringTy; ValArrayTy intTy;
		stringTy; ValArrayTy (TupleTy [stringTy; TagOfTy (namedTy "GLVal"); intTy]);
		ValArrayTy stringTy
	], shaderTy);
	("newFrameBuffer", ExternalSym [], [stringTy; stringTy; intTy; intTy;
		ValArrayTy (namedTy "BufferType");
		stringTy; ValArrayTy (TupleTy [stringTy; TagOfTy (namedTy "GLVal"); intTy]);
		ValArrayTy stringTy
	], fboTy);
	("setUniform", ExternalSym [], [shaderTy; intTy; namedTy "GLVal"], unitTy);
	("setUniform", ExternalSym [], [fboTy; intTy; namedTy "GLVal"], unitTy);
	("loadTexture", ExternalSym [], [shaderTy; intTy; fboTy; intTy], unitTy);
	("loadTexture", ExternalSym [], [fboTy; intTy; fboTy; intTy], unitTy);
	("render", ExternalSym [], [shaderTy; renderDataTy], unitTy);
	("render", ExternalSym [], [fboTy; renderDataTy], unitTy);
	("renderQuad", ExternalSym [], [shaderTy], unitTy);
	("renderQuad", ExternalSym [], [fboTy], unitTy);

		(* render data *)
	("renderData", ExternalSym [], [], renderDataTy);
	("alloc", ExternalSym [], [renderDataTy; intTy], unitTy);
	("get", ExternalSym [], [renderDataTy; intTy], namedTy "GLVal");
	("set", ExternalSym [], [renderDataTy; intTy; namedTy "GLVal"], unitTy);

		(* vec3 *)
		(* mat4 *)
	("newMat4", ExternalSym [], [], mat4Ty);
	("idMat4", ExternalSym [], [mat4Ty], unitTy);
	("translate", ExternalSym [], [mat4Ty; floatTy; floatTy; floatTy], unitTy);
	("rotateX", ExternalSym [], [mat4Ty; floatTy], unitTy);
	("lookAt", ExternalSym [], [mat4Ty; vec3Ty; vec3Ty; vec3Ty], unitTy);
]

let sulfurTypes = [
	(pb "Glyph", TDefVD (UnionTD [
		(qn "GNop", [], GlobalEB "C_NOP");
		(qn "GBox", [intTy; intTy; intTy; intTy; intTy], GlobalEB "C_BOX");
		(qn "GSprite", [intTy; intTy; spriteTy; intTy], GlobalEB "C_SPRITE");
		(qn "GText", [spriteTy; stringTy], GlobalEB "C_TEXT")
	]));
	(*(QT None, "Glyph3d", TDefVD (UnionTD [
		("G3Nop", [], GlobalEB "C3_NOP");
		("G3QuadX", [floatTy; floatTy; floatTy; spriteTy; intTy], GlobalEB "C3_QX");
		("G3QuadY", [floatTy; floatTy; floatTy; spriteTy; intTy], GlobalEB "C3_QY");
		("G3QuadZ", [floatTy; floatTy; floatTy; spriteTy; intTy], GlobalEB "C3_QZ");
	]));*)
	(pb "GLVal", TDefVD (UnionTD [
		(qn "GLFloat", [floatTy], GlobalEB "C_GL_FLOAT");
		(qn "GLMat4", [mat4Ty], GlobalEB "C_GL_MAT4");
	]));
	(*(QT None, "GLType", TDefVD (UnionTD [
		("GLFloat", [], Some "C_GL_FLOAT");
		("GLMat4", [], Some "C_GL_MAT4");
	]));
	(QT None, "GLVal", TDefVD (UnionTD [
		("GLFloatV", [floatTy], Some "C_GL_FLOAT");
		("GLMat4V", [mat4Ty], Some "C_GL_MAT4");
	]));*)
	(pb "BufferType", TDefVD (EnumTD(false, [
		(qn "FBOColor", GlobalEB "C_FBO_COLOR");
		(qn "FBODepth", GlobalEB "C_FBO_DEPTH");
		(qn "FBORender", GlobalEB "C_FBO_RENDER")
	])))
]

	(*
		builtin tree map
	*)

let builtinTreeMap (): (m_virt_bind list) tree_map =
	let m1 = single_tree ["builtin"] (toVirtList builtinList) in
	let m2 = add_tree m1 ["Sys"; "Os"] (toVirtList osList) in
	let m3 = add_tree m2 ["Sys"; "Input"] (toVirtList inputList) in
	add_tree m3 ["Sys"; "Sulfur"] (sulfurTypes @ (toVirtList sulfurList))

let extractSymbols (symList: (raw_bind, qual_name) virt_bind list): raw_bind list =
	List.concat (List.map (fun (f, vd) -> match vd with
		TDefVD (EnumTD(_, cl)) -> f :: (List.map (fun (QN(_, c), _) -> RawBind c) cl)
		| TDefVD (UnionTD cl) -> f :: (List.map (fun (QN(_, c), _, _) -> RawBind c) cl)
		| _ -> [f]
	) symList)

(*
		primitive flag - flag indicating that the canonical name should be unqualified 


type prim_flag = PF | NPF	*)