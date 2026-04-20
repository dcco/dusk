open Commons.Try_log
open Parser.Lex_token
open Parser.Dusk_type
open Codegen.Fin_type

	(* error types *)

type tc_err =
	BadType_Err of g_type * g_type * l_pos
	| MismatchedArgNum_Err of int * int * l_pos
		(* functions *)
	| NoOverload_Err of string * g_type option * l_pos
		(* tuples *)
	| NonTuple_Err of g_type * l_pos
	| TupleIndexOOB_Err of g_type * int * l_pos
		(* tag tuples *)
	| NonCtor_Err of string * l_pos
		(* arrays *)
	| MismatchedArrayDim_Err of int list * int * int * l_pos
	| NestedFormat_Err of l_pos
		(* structs *)
	| NonStruct_Err of g_type * l_pos
	| BadCtorStruct_Err of string * l_pos
	| MissingField_Err of string * string * l_pos
	| BadField_Err of string * string * l_pos
		(* returns *)
	| EarlyReturn_Err of string * l_pos
	| NoReturn_Err of string * l_pos
		(* constant evaluation *)
	| BadCLoad_Err of string * l_pos
	| FailCLoad_Err of l_pos

type 'a tc_res = ('a, tc_err) try_res

let string_of_first_arg (t: g_type option): string = match t with
	None -> "empty arg list"
	| Some tau -> "type \"" ^ (string_of_type tau) ^ "\""

let string_of_tc_err (e: tc_err) = match e with
	| BadType_Err(s, t, p) -> "Expected type \"" ^ (string_of_type t) ^ "\" and received type \"" ^
		(string_of_type s) ^ "\" at " ^ (string_of_pos p) ^ "."
	| MismatchedArgNum_Err(s, t, p) -> "Function call expected " ^ (string_of_int t) ^ " arguments and received " ^
		(string_of_int s) ^ " at " ^ (string_of_pos p) ^ "."
	| NoOverload_Err(f, t, p) -> "Function \"" ^ f ^ "\" does not have overload for " ^
		(string_of_first_arg t) ^ " at " ^ (string_of_pos p) ^ "."
	| NonTuple_Err(t, p) -> "Tuple operation called on non-tuple type \"" ^ (string_of_type t) ^ "\" at " ^ (string_of_pos p) ^ "."
	| TupleIndexOOB_Err(t, i, p) -> "Attempted to access index " ^ (string_of_int i ) ^ " of tuple type \"" ^
		(string_of_type t) ^ "\" at " ^ (string_of_pos p) ^ "."
	| NonCtor_Err(t, p) -> "Type name \"" ^ t ^ "\" did not resolve to a constructor at " ^ (string_of_pos p) ^ "."
	| MismatchedArrayDim_Err(dim_l, dim_prod, n, p) ->
		"Array initializer has " ^ (string_of_int n) ^ " elements, but specified dimensions [" ^
		(String.concat ", " (List.map string_of_int dim_l)) ^ "] require " ^
		(string_of_int dim_prod) ^ " at " ^ (string_of_pos p) ^ "." 
	| NestedFormat_Err p  -> "Variable-size array constructor found outside of top-level assignment at " ^ (string_of_pos p) ^ "."
	| NonStruct_Err(t, p) -> "Struct operation called on non-struct type \"" ^ (string_of_type t) ^ "\" at " ^ (string_of_pos p) ^ "."
	| BadCtorStruct_Err(t, p) -> "Attempted to initialize struct with non-struct constructor \"" ^ t ^ "\" at " ^ (string_of_pos p) ^ "."
	| MissingField_Err(t, x, p) -> "Initialization of struct \"" ^ t ^
		"\" missing field \"" ^ x ^ "\" at " ^ (string_of_pos p) ^ "."
	| BadField_Err(t, x, p) -> "Attempted to read non-existent field \"" ^ x ^
		"\" from struct \"" ^ t ^ "\" at " ^ (string_of_pos p) ^ "."
	| EarlyReturn_Err(f, p) ->
		"Early return from function \"" ^ f ^ "\" producing unreachable code at " ^ (string_of_pos p) ^ "."
	| NoReturn_Err(f, p) ->
		"Declaration of function \"" ^ f ^ "\" does not return on all paths at " ^ (string_of_pos p) ^ "."
	| BadCLoad_Err(f, p) ->
		"Failed cLoad, could not find file \"" ^ f ^ "\" at " ^ (string_of_pos p) ^ "."
	| FailCLoad_Err p -> "Failed cLoad at " ^ (string_of_pos p) ^ "."
