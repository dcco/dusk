
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
	| NamedTy of 'm * string
	| TupleTy of 'm raw_type list
	| ArrayTy of int * 'm raw_type

type 'm fun_type = 'm raw_type list * 'm raw_type

type m_type = qual_tag raw_type

let rec string_of_type (tau: 'm raw_type): string = match tau with
	| PrimTy x -> x
	| NamedTy(_, x) -> x
	| TupleTy tau_l -> "(" ^ String.concat ", " (List.map string_of_type tau_l) ^ ")"
	| ArrayTy(i, tau) -> (string_of_int i) ^ "d[" ^ (string_of_type tau) ^ "]"

let string_of_fun_type ((tau_pl, tau_r): 'm fun_type): string =
	"fn(" ^ (String.concat ", " (List.map string_of_type tau_pl)) ^ ") " ^ (string_of_type tau_r)

	(* - auxiliary function, used to find the "first" argument of a function type *)

let hd_opt (l: 'a list): 'a option = match l with
	[] -> None
	| v :: _ -> Some v

	(* primitive types *)

let primTy x = PrimTy x
let namedTy x = NamedTy(QT None, x)

let unitTy = primTy "Unit"
let intTy = primTy "Int"
let floatTy = primTy "Float"
let stringTy = primTy "String"
let boolTy = primTy "Bool"

	(*
		user-defined types:
		- enum
	*)

type 'm enum_case = string * 'm raw_type list * string option

type 'm raw_tdef =
	EnumTD of ('m enum_case) list

type m_tdef = qual_tag raw_tdef