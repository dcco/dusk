open Llvm
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
	| TupleTy tau_l -> struct_type context (Array.of_list (List.map (genType env) tau_l))
	| _ -> failwith "BUG: gen_type.ml - Unimplemented type."

let genFunType (env: dusk_env) (pl: (string * g_type) list) (tau_r: g_type): lltype =
	function_type (genType env tau_r) (Array.of_list (List.map (fun (_, t) -> genType env t) pl))