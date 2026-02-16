open Commons.Tree_map
open Parser.Dusk_type

	(*
		builtin symbol / virtual declarations
		> symbol includes the binding type, so that it can be called / compiled
		-- binary expression ASM
		-- external bindings

		> virtual dec adds the type, giving enough info to compile end-to-end
	*)

type sym =
	BinaryASMSym of string
	| ExternalSym
	| UserDefSym

type t_sym = sym * string fun_type

type virt_dec = string * sym * string fun_type

	(*
		builtins
	*)

let toSymList (rawList: (string * sym * string raw_type list * string raw_type) list): virt_dec list =
	List.map (fun (x, v, tau_pl, tau_r) -> (x, v, (tau_pl, tau_r))) rawList

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
	("gt", BinaryASMSym "igt", [intTy; intTy], boolTy)
]

let osList = [
	("print", ExternalSym, [stringTy], unitTy)
]

	(*
		builtin tree map
	*)

let builtinTreeMap (): (virt_dec list) tree_map =
	let m = single_tree ["Predef"] (toSymList builtinList) in
	add_tree m ["Sys"; "Os"] (toSymList osList)

let builtinQualList (): (string list * virt_dec) list =
	List.concat (List.map (fun (path, vdl) ->
		List.map (fun vd -> (path, vd)) vdl
	) (flatten_tree (builtinTreeMap ())))
