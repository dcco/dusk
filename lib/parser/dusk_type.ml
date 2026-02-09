
	(*
		dusk types:
		> 'a: used to determine the type of "identifier used" (raw names vs canonical names) 
		- primitive -- Unit, Int, Float, Bool, String
		- named type
		- tuple type -- (tau, ...)
		- array type -- 1d[tau], 2d[_], etc
	*)

type 'a raw_type =
	PrimTy of string
	| NamedTy of 'a
	| TupleTy of 'a raw_type list
	| ArrayTy of int * 'a raw_type

type m_type = string raw_type

let rec string_of_type (tau: m_type): string = match tau with
	| PrimTy x -> x
	| NamedTy x -> x
	| TupleTy tau_l -> "(" ^ String.concat ", " (List.map string_of_type tau_l) ^ ")"
	| ArrayTy(i, tau) -> (string_of_int i) ^ "d[" ^ (string_of_type tau) ^ "]"

	(* primitive types *)

let primTy x = PrimTy x
let namedTy x = NamedTy x

let unitTy = primTy "Unit"
let intTy = primTy "Int"
let floatTy = primTy "Float"
let stringTy = primTy "String"
let boolTy = primTy "Bool"

	(*
		user-defined types:
		- enum
	*)

type 'a enum_case = 'a * 'a raw_type list * string option

type 'a raw_tdef =
	EnumTD of ('a enum_case) list

type m_tdef = string raw_tdef