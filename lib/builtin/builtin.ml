open Commons.Tree_map
open Parser.Dusk_type

	(*
		builtin symbol / virtual declarations
		> symbol includes the binding type, so that it can be called / compiled
		-- binary expression ASM
		-- external bindings
		>>> list of integers indicating "virtual" arguments, which arguments must be wrapped in ptrs 

		> virtual dec adds the type, giving enough info to compile end-to-end
	*)

type sym =
	BinaryASMSym of string
	| ExternalSym of int list
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
	("drawTX", ExternalSym [0], [TupleTy [intTy; intTy; intTy; intTy]], unitTy)
]

	(*
		builtin tree map
	*)

let builtinTreeMap (): (virt_dec list) tree_map =
	let m1 = single_tree ["builtin"] (toSymList builtinList) in
	let m2 = add_tree m1 ["Sys"; "Os"] (toSymList osList) in
	add_tree m2 ["Sys"; "Sulfur"] (toSymList sulfurList)

let builtinQualList (): (string list * virt_dec) list =
	List.concat (List.map (fun (path, vdl) ->
		List.map (fun vd -> (path, vd)) vdl
	) (flatten_tree (builtinTreeMap ())))
