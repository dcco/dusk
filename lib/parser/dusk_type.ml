
	(*
		identifier tag types:
		- qualified -- raw, module-prefixed identifier taken from parsing
		- canon -- identifier after canonization 
	*)

type qual_tag = QT of string option
type canon_tag = CT

	(*
		dusk types:
		> 'a: used to determine the type of "identifier used" (raw names vs canonical names) 
		- primitive -- Unit, Int, Float, Bool, String
		- named type
		- tuple type -- (tau, ...)
		- array type -- 1d[tau], 2d[_], etc
	*)

type 'm raw_type =
	PrimTy of string
	| BuiltinTy of string
	| NamedTy of 'm * string
	| TupleTy of 'm raw_type list
	| ArrayTy of int * 'm raw_type
	| ValArrayTy of 'm raw_type
	| TagOfTy of 'm raw_type
	| FunTy of 'm raw_type list * 'm raw_type
	| BotTy

type 'm fun_type = 'm raw_type list * 'm raw_type

type m_type = qual_tag raw_type

let rec string_of_type (tau: 'm raw_type): string = match tau with
	| PrimTy x -> x
	| BuiltinTy x -> x
	| NamedTy(_, x) -> x
	| TupleTy tau_l -> "(" ^ String.concat ", " (List.map string_of_type tau_l) ^ ")"
	| ArrayTy(i, tau) -> (string_of_int i) ^ "d[" ^ (string_of_type tau) ^ "]"
	| ValArrayTy tau -> "1v[" ^ (string_of_type tau) ^ "]"
	| TagOfTy tau -> (string_of_type tau) ^ ".t"
	| FunTy(tau_pl, tau_r) -> string_of_fun_type (tau_pl, tau_r)
	| BotTy -> "BOT"
	
and string_of_fun_type ((tau_pl, tau_r): 'm fun_type): string =
	"Fn(" ^ (String.concat ", " (List.map string_of_type tau_pl)) ^ ") " ^ (string_of_type tau_r)

	(* - auxiliary function, used to find the "first" argument of a function type *)

let hd_opt (l: 'a list): 'a option = match l with
	[] -> None
	| v :: _ -> Some v

	(* primitive types *)

let primTy x = PrimTy x
let builtinTy x = BuiltinTy x
let namedTy x = NamedTy(QT None, x)

let unitTy = primTy "Unit"
let intTy = primTy "Int"
let floatTy = primTy "Float"
let stringTy = primTy "String"
let boolTy = primTy "Bool"

let uint8Ty = primTy "Uint8"
let uint32Ty = primTy "Uint32"
let uint64Ty = primTy "Uint64"
let keyTy = primTy "Key"

	(*
		user-defined types:
		- enum
	*)

type enum_back = NoEB | IntEB of int | GlobalEB of string

type 'm field_list = (string * 'm raw_type) list

type enum_case = string * enum_back
type 'm union_case = string * 'm raw_type list * enum_back

type 'm raw_tdef =
	StructTD of 'm field_list
	| EnumTD of bool * enum_case list
	| UnionTD of ('m union_case) list

type m_field_list = qual_tag field_list
type m_tdef = qual_tag raw_tdef