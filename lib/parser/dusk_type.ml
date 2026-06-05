
	(*
		identifier types:
		* qualified - raw, module-prefixed identifier taken from parsing
		* canon - identifier after canonization (also contains pre-canonized name for debugging)
	*)

type qual_name = QN of string option * string
type canon_name = CN of string * string list

let qn x = QN(None, x)
let cn (QN(xo, x)) x' = match xo with
	None -> CN(x', [x])
	| Some p -> CN(x', [p; x])
 
let cr (CN(x, _)) = x

(*
type qual_tag = QT of string option
type canon_tag = CT*)

	(*
		dusk types:
		> 'm: used to determine the type of "identifier used" (raw names vs canonical names) 
		- primitive -- Unit, Int, Float, Bool, String
		- named type
		- tuple type -- (tau, ...)
		- array type -- 1d[tau], 2d[_], etc
	*)

type 'm raw_type =
	PrimTy of string
	| BuiltinTy of string
	| NamedTy of 'm
	| TupleTy of 'm raw_type list
	| TagTupleTy of 'm * 'm raw_type list
	| ArrayTy of int * 'm raw_type
	| TagOfTy of 'm raw_type
	| FunTy of 'm raw_type list * 'm raw_type
	| NullableTy of 'm raw_type
	| NullTy
	| BotTy
		(* to replace with polymorphism *)
	| ArrayGenTy

type 'm fun_type = 'm raw_type list * 'm raw_type

type m_type = qual_name raw_type

	(* string functions *)

let rec string_of_type (f: 'm -> string) (tau: 'm raw_type): string = match tau with
	| PrimTy x -> x
	| BuiltinTy x -> x
	| NamedTy x -> f x
	| TupleTy tau_l -> "(" ^ String.concat ", " (List.map (string_of_type f) tau_l) ^ ")"
	| TagTupleTy(x, _) -> f x
	| ArrayTy(i, tau) -> (string_of_int i) ^ "d[" ^ (string_of_type f tau) ^ "]"
	| TagOfTy tau -> (string_of_type f tau) ^ ".t"
	| FunTy(tau_pl, tau_r) -> string_of_fun_type f (tau_pl, tau_r)
	| NullableTy tau -> (string_of_type f tau) ^ "?"
	| NullTy -> "NULL"
	| BotTy -> "BOT"
	| ArrayGenTy -> "1d[_]"
	
and string_of_fun_type (f: 'm -> string) ((tau_pl, tau_r): 'm fun_type): string =
	"Fn(" ^ (String.concat ", " (List.map (string_of_type f) tau_pl)) ^ ") " ^ (string_of_type f tau_r)

	(* - auxiliary function, used to find the "first" argument of a function type *)

let hd_opt (l: 'm list): 'm option = match l with
	[] -> None
	| v :: _ -> Some v

	(* primitive types *)

let primTy x = PrimTy x
let builtinTy x = BuiltinTy x
let namedTy x = NamedTy (QN(None, x))

let unitTy = primTy "Unit"
let intTy = primTy "Int"
let floatTy = primTy "Float"
let stringTy = primTy "String"
let boolTy = primTy "Bool"

let uint8Ty = primTy "U8"
let uint32Ty = primTy "U32"
let uint64Ty = primTy "U64"
let keyTy = primTy "Key"

	(*
		user-defined types:
		- enum
	*)

type enum_back = NoEB | IntEB of int | GlobalEB of string

type 'm field_list = (string * 'm raw_type) list

type 'm enum_case = 'm * enum_back
type 'm union_case = 'm * 'm raw_type list * enum_back

type 'm raw_tdef =
	StructTD of 'm field_list
	| EnumTD of bool * 'm enum_case list
	| UnionTD of ('m union_case) list

type m_field_list = qual_name field_list
type m_tdef = qual_name raw_tdef