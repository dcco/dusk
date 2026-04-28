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
let i64Type = i64_type context
let fType = float_type context
let bType = i1_type context
let ptrType = pointer_type context;;

	(* - complex types *)

let structType tau_l = struct_type context (Array.of_list tau_l);;

let gcArrType = struct_type context (Array.of_list [iType; iType; ptrType]);;

let gcDimsType n = struct_type context (Array.of_list (List.init n (fun _ -> iType)));;

let gcFullArrType n =
	let l = [iType; iType; ptrType] @ (List.init n (fun _ -> iType)) in
	struct_type context (Array.of_list l);;

	(*
		code generation related to types
	*)

let isHeapType (env: dusk_env) (tau: g_type): bool = match tau with
	NamedTy(_, x) -> (match Hashtbl.find_opt env (DTName x) with
		Some (DTDef td) -> (match td with
			StructTD_C(_, _) -> true
			| _ -> false
		)
		| _ -> failwith "BUG: gen_type.ml - Invalid type encountered while checking heap type status."
	)
	| ArrayTy(_, _) -> true
	| _ -> false

let genType (tau: g_type): lltype = match tau with
	PrimTy "Unit" -> voidType
	| PrimTy "Int" -> iType
	| PrimTy "Float" -> fType
	| PrimTy "Bool" -> bType
	| PrimTy "String" -> ptrType
	| PrimTy "Long" -> i64Type
	| PrimTy "Key" -> iType
	| BuiltinTy _ -> ptrType
	(*| TupleTy tau_l -> struct_type context (Array.of_list (List.map (genType env) tau_l)) *)
	| NamedTy(_, _) -> ptrType
	(*match Hashtbl.find_opt env (DTName x) with
		Some (DTDef td) -> ptrType
		(*match td with
			OpaqueTD_C(_, _) -> ptrType (*array_type i8Type i*)
			| StructTD_C(_, _) -> ptrType
		*)
		| _ -> failwith ("BUG: gen_type.ml - Generation with non-existent type \"" ^ x ^ "\"")
	*)
	| TupleTy _ -> ptrType
	| ArrayTy(_, _) -> ptrType
	| ValArrayTy _ -> ptrType
	| BotTy -> ptrType
	| _ -> failwith "BUG: gen_type.ml - Unimplemented type."

let rec genInnerType (env: dusk_env) (tau: g_type): lltype = match tau with
	TupleTy tau_l -> struct_type context (Array.of_list (List.map (genInnerType env) tau_l))
	| tau -> genType tau

let genAlign (env: dusk_env) (tau: g_type): int option = match tau with
	NamedTy(_, x) -> (match Hashtbl.find_opt env (DTName x) with
		Some (DTDef td) -> (match td with
			OpaqueTD_C(_, align) -> Some align
			| _ -> None
		)
		| _ -> None
	)
	| _ -> None

let genAllocaType (env: dusk_env) (tau: g_type): lltype = match tau with
	TupleTy tau_l -> struct_type context (Array.of_list (List.map genType tau_l))
	| NamedTy(_, x) -> (match Hashtbl.find_opt env (DTName x) with
		Some (DTDef td) -> (match td with
			OpaqueTD_C(i, _) -> array_type i8Type i
			| _ -> failwith ("BUG: gen_type.ml - Box allocation made for invalid type \"" ^ x ^ "\".")
		)
		| _ -> failwith ("BUG: gen_type.ml - Generation with non-existent type \"" ^ x ^ "\"")
	)
	| _ -> failwith "BUG: gen_type.ml - Box allocation made for invalid type."

	(*match Hashtbl.find_opt env (DTName x) with
		Some (DTDef td) -> ptrType
		(*match td with
			OpaqueTD_C(_, _) -> ptrType (*array_type i8Type i*)
			| StructTD_C(_, _) -> ptrType
		*)
		| _ -> failwith ("BUG: gen_type.ml - Generation with non-existent type \"" ^ x ^ "\"")
	*)
	
let genFunType (pl: (string * g_type) list) (tau_r: g_type): lltype =
	function_type (genType tau_r) (Array.of_list (List.map (fun (_, t) -> genType t) pl))

let virtTagTupleType (tau_l: g_type list): lltype =
	let tau_l' = List.map genType tau_l in
	struct_type context (Array.of_list (i8Type :: tau_l'))

	(*
		size/alignment of a type
	*)

let align_rup (offset: int) (align: int): int = ((offset + align - 1) / align) * align

let size_of_type (cont: llvm_cont) (t: lltype): int = Int64.to_int (DataLayout.abi_size t cont.data_layout)

let align_of_type (cont: llvm_cont) (t: lltype): int = DataLayout.abi_align t cont.data_layout