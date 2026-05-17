open Llvm
open Llvm_target

open Parser.Dusk_type
open Fin_type
open Gen_cont

	(*
		LLVM types
		- basic types
	*)

let voidType = void_type context
let i8Type = i8_type context
let tagType = i16_type context
let iType = i32_type context
let i64Type = i64_type context
let fType = float_type context
let bType = i1_type context
let ptrType = pointer_type context;;

	(* - complex types *)

let structType tau_l = struct_type context (Array.of_list tau_l);;

let gcArrType = struct_type context (Array.of_list [iType; iType; iType; ptrType]);;

let gcDimsType n = struct_type context (Array.of_list (List.init n (fun _ -> iType)));;

	(*
		genType: gives the "main" type for a datatype
			(the one handled in expressions / used in arguments)
	*)

let genType (debug: string) (env: dusk_env) (tau: g_type): lltype = match tau with
	PrimTy "Unit" -> failwith ("BUG: gen_type.ml - Unit type used outside of return type. " ^ debug)
	| PrimTy "Int" -> iType
	| PrimTy "Float" -> fType
	| PrimTy "Bool" -> bType
	| PrimTy "String" -> ptrType
	| PrimTy "U8" -> i8Type
	| PrimTy "U32" -> iType
	| PrimTy "U64" -> i64Type
	| PrimTy "Key" -> iType
	| PrimTy x -> failwith ("BUG: gen_type.ml - Non-existent primitive type \"" ^ x ^ "\". " ^ debug)
	| BuiltinTy _ -> ptrType
	| NamedTy t -> (match Hashtbl.find_opt env (DTName (cr t)) with
		Some (DTDef td) -> (match td with
			EnumTD_C -> tagType
			| _ -> ptrType
		) 
		| _ -> failwith ("BUG: gen_type.ml - Invalid type \"" ^ (cr t) ^ "\" encountered while generating type. " ^ debug)
	)
	| TupleTy _ -> ptrType
	| ArrayTy(_, _) -> ptrType
	| ValArrayTy _ -> ptrType
	| TagOfTy _ -> tagType
	| FunTy _ -> ptrType
	| BotTy -> ptrType

let genTagTupleType (env: dusk_env) (tau_l: g_type list): lltype =
	let tau_l' = List.map (genType "(Tag Tuple)" env) tau_l in
	struct_type context (Array.of_list (tagType :: tau_l'))

	(*
		genStoreType: gives the type a datatype should be "stored" with.
			(generally the same as main type, except for tuples/unions,
			which are stored as hard copies)
	*)

type store_type =
	TStore of lltype
	| CopyStore of lltype * int option

let genStoreTypeFull (debug: string) (env: dusk_env) (tau: g_type): store_type = match tau with
	PrimTy "Unit" -> TStore voidType
	| TupleTy tau_l -> CopyStore (struct_type context (Array.of_list (List.map (genType debug env) tau_l)), None)
	| NamedTy t -> (match Hashtbl.find_opt env (DTName (cr t)) with
		Some (DTDef td) -> (match td with
			EnumTD_C -> TStore tagType
			| OpaqueTD_C(i, align) -> CopyStore (array_type i8Type i, Some align)
			| _ -> TStore ptrType
		) 
		| Some v -> failwith ("BUG: gen_type.ml - Invalid type \"" ^ (cr t) ^
			"\" mapped to unexpected value " ^ (string_of_dval v) ^ " while generating storage type.")
		| None -> failwith ("BUG: gen_type.ml - Invalid type \"" ^ (cr t) ^ "\" encountered while generating storage type.")
	)
	| _ -> TStore (genType debug env tau)

let genStoreType (debug: string) (env: dusk_env) (tau: g_type): lltype * int option =
	match genStoreTypeFull debug env tau with
		TStore t -> (t, None)
		| CopyStore(t, alignOpt) -> (t, alignOpt)

let genFunType (debug: string) (env: dusk_env) (tau_pl: g_type list) (tau_r: g_type): lltype =
	let tau_pl' = List.map (genType debug env) tau_pl in
	match genStoreTypeFull debug env tau_r with
		TStore tau_r' -> function_type tau_r' (Array.of_list tau_pl')
		| CopyStore(_, _) -> function_type voidType (Array.of_list (ptrType :: tau_pl'))

	(*
		genDerefType: gives the underlying type for dusk pointer types
			(used for struct/array RW operations)
	*)

let genDerefType (debug: string) (env: dusk_env) (tau: deref_type): lltype = match tau with
	TypeDeref (TupleTy tau_l) -> struct_type context (Array.of_list (List.map (genType debug env) tau_l))
	| TypeDeref (NamedTy t) -> (match Hashtbl.find_opt env (DTName (cr t)) with
		Some (DTDef td) -> (match td with
			EnumTD_C -> tagType
			| OpaqueTD_C(_, _) ->
				failwith ("BUG: gen_type.ml - Ambiguous union type \"" ^ (cr t) ^ "\" encountered while dereferencing type. " ^ debug)
			| StructTD_C(tl', _) -> struct_type context (Array.of_list tl') 
		) 
		| _ -> failwith ("BUG: gen_type.ml - Invalid type \"" ^ (cr t) ^ "\" encountered while dereferencing type." ^ debug)
	)
	| CtorDeref t -> (match Hashtbl.find_opt env (DCtor t) with
		Some (DEnum(_, tau_l)) -> struct_type context (Array.of_list tau_l)
		| _ -> failwith ("BUG: gen_type.ml - Could not find enum definition for \"" ^ t ^ "\" encountered while dereferencing type." ^ debug)
	)
	| _ -> failwith ("BUG: gen_type.ml - Invalid/non-pointer type encountered while dereferencing type." ^ debug)

	(*
		garbage collection layout type
		- gcElemType: gives the gc layout for an array element (given as a list of search criterion)
			- direct: searches directly through the given pointer
			- offset: searches at the given offset
			X (DEPRECATED, we're simply not allowing variant offsets) variant: looks up offsets using the variant tag
	*)

type gc_child = DirectChild | OffsetChild of int

let offsetChildList (childList: gc_child list) (offset: int): gc_child list =
	List.map (fun c -> match c with
		DirectChild -> OffsetChild offset
		| OffsetChild i -> OffsetChild (offset + i)
	) childList

let rec gcElemType (cont: llvm_cont) (env: dusk_env) (tau: g_type): gc_child list = match tau with
	NamedTy t -> (match Hashtbl.find_opt env (DTName (cr t)) with
		Some (DTDef td) -> (match td with
			StructTD_C(_, _) -> [DirectChild]
			| OpaqueTD_C(_, _) -> []
			| _ -> []
		) 
		| _ -> failwith ("BUG: gen_type.ml - Invalid type \"" ^ (cr t) ^ "\" encountered while constructing GC layout for type.")
	)
	| PrimTy "String" -> [DirectChild]
	| ArrayTy(_, _) -> [DirectChild]
	| TupleTy tau_l -> gcElemTypeList cont env 0 tau_l
	| ValArrayTy _ -> failwith ("BUG: gen_type.ml - Value-arrays cannot be stored in other data structures.")
	| _ -> []
and gcElemTypeList (cont: llvm_cont) (env: dusk_env) (fieldOffset: int) (tau_l: g_type list): gc_child list =
	let tau_s = struct_type context (Array.of_list (List.map (genType "(GC Layout)" env) tau_l)) in
	let childList = ref [] in
	List.iteri (fun i tau ->
		let offset = Int64.to_int (DataLayout.offset_of_element tau_s (i + fieldOffset) cont.data_layout) in
		let childElem = offsetChildList (gcElemType cont env tau) offset in
		childList := !childList @ childElem
	) tau_l; !childList

	(*
		size/alignment of a type
	*)

let align_of_type (cont: llvm_cont) (t: lltype): int = DataLayout.abi_align t cont.data_layout

let align_rup (offset: int) (align: int): int = ((offset + align - 1) / align) * align

let size_of_type (cont: llvm_cont) (t: lltype): int = Int64.to_int (DataLayout.abi_size t cont.data_layout)

	(*
		garbage collection / storage type functions
	*)
(*
let isHeapType (env: dusk_env) (tau: g_type): bool = match tau with
	NamedTy(_, x) -> (match Hashtbl.find_opt env (DTName x) with
		Some (DTDef td) -> (match td with
			StructTD_C(_, _) -> true
			| _ -> false
		)
		| _ -> failwith ("BUG: gen_type.ml - Invalid type \"" ^ x ^ "\" encountered while checking heap type status.")
	)
	| ArrayTy(_, _) -> true
	| _ -> false
*)

	(*| TupleTy tau_l -> struct_type context (Array.of_list (List.map (genType env) tau_l)) *)
	(*match Hashtbl.find_opt env (DTName x) with
		Some (DTDef td) -> ptrType
		(*match td with
			OpaqueTD_C(_, _) -> ptrType (*array_type i8Type i*)
			| StructTD_C(_, _) -> ptrType
		*)
		| _ -> failwith ("BUG: gen_type.ml - Generation with non-existent type \"" ^ x ^ "\"")
	*)
(*
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
	
let genFunType (pl: (string * g_type) list) (tau_r: g_type): lltype =
	function_type (genType tau_r) (Array.of_list (List.map (fun (_, t) -> genType t) pl))

let virtTagTupleType (tau_l: g_type list): lltype =
	let tau_l' = List.map genType tau_l in
	struct_type context (Array.of_list (tagType :: tau_l'))

*)