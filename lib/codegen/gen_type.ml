open Llvm
open Llvm_target

open Fin_type
open Gen_cont

	(*
		basic LLVM types
	*)

let voidType = void_type context
let i8Type = i8_type context
let iType = i32_type context
let fType = float_type context
let bType = i1_type context
let ptrType = pointer_type context;;

	(* - complex types *)

let structType tau_l = struct_type context (Array.of_list tau_l);;

	(*
		code generation related to types
	*)

let rec genType (env: dusk_env) (tau: g_type): lltype = match tau with
	PrimTy "Unit" -> voidType
	| PrimTy "Int" -> iType
	| PrimTy "Float" -> fType
	| PrimTy "Bool" -> bType
	| PrimTy "String" -> ptrType
	| PrimTy "1d" -> ptrType
	| PrimTy "nd" -> ptrType
	| BuiltinTy _ -> ptrType
	| TupleTy tau_l -> struct_type context (Array.of_list (List.map (genType env) tau_l))
	| NamedTy(_, x) -> (match Hashtbl.find_opt env (DTName x) with
		Some (DTDef td) -> (match td with
			OpaqueTD_C(i, _) -> array_type i8Type i
			| StructTD_C(_, _) -> ptrType
		)
		| _ -> failwith ("BUG: gen_type.ml - Generation with non-existent type \"" ^ x ^ "\"")
	)
	| _ -> failwith "BUG: gen_type.ml - Unimplemented type."

let genAlign (env: dusk_env) (tau: g_type): int option = match tau with
	NamedTy(_, x) -> (match Hashtbl.find_opt env (DTName x) with
		Some (DTDef td) -> (match td with
			OpaqueTD_C(_, align) -> Some align
			| _ -> None
		)
		| _ -> None
	)
	| _ -> None

let genFunType (env: dusk_env) (pl: (string * g_type) list) (tau_r: g_type): lltype =
	function_type (genType env tau_r) (Array.of_list (List.map (fun (_, t) -> genType env t) pl))

let genTagTupleType (env: dusk_env) (tau_l: g_type list): lltype =
	let tau_l' = List.map (genType env) tau_l in
	struct_type context (Array.of_list (i8Type :: tau_l'))

	(*
		size/alignment of a type
	*)

let align_rup (offset: int) (align: int): int = ((offset + align - 1) / align) * align

let size_of_type (cont: llvm_cont) (t: lltype): int = Int64.to_int (DataLayout.abi_size t cont.data_layout)

let align_of_type (cont: llvm_cont) (t: lltype): int = DataLayout.abi_align t cont.data_layout