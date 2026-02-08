open Lex_token
open Dusk_type

	(*
		######################
		## main AST definition
		######################
	*)

type const =
	IConst of int
	| FConst of float
	| SConst of string
	| BConst of bool

	(* functions parameterized by something other than a pure value *)
type x_op =
		(* previously, ElemOp *)
	TupleIndexOp of int
	| ArrayIndexOp of int
	| LengthOp
	| UpdateOp

type ('m, 'ann) exp =
	ConstExp of const * 'ann
		(* selectors *)
	| VarExp of 'm * string * 'ann
	| OpExp of x_op * 'ann
		(* ctors *)
	| TupleExp of ('m * string) option * ('m, 'ann) exp list * 'ann
	| NewDimExp of int * m_type * ('m, 'ann) exp list * 'ann
		(* function call *)
	| AppExp of ('m, 'ann) exp * ('m, 'ann) exp list * 'ann

type pat =
	VarPat of string
	| ListPat of (string option) list 

type range_type = LtRange | LeqRange | ListRange

type ('m, 'ann) stmt =
	EvalStmt of ('m, 'ann) exp * 'ann
	| AssignStmt of string * ('m, 'ann) exp * 'ann
	| ReturnStmt of ('m, 'ann) exp option * 'ann
	| PatStmt of pat * ('m, 'ann) exp * 'ann
	| IfStmt of ('m, 'ann) exp * ('m, 'ann) stmt list * ('m, 'ann) stmt list * 'ann
	| WhileStmt of ('m, 'ann) exp * ('m, 'ann) stmt list * 'ann
	| ForStmt of string * range_type * ('m, 'ann) exp * ('m, 'ann) stmt list * 'ann

	(*
		######################
		## position annotation
		######################
	*)

type qual_prefix = string option

type n_exp = (qual_prefix, l_pos) exp
type n_stmt = (qual_prefix, l_pos) stmt

	(* annotation reader *)

let ann_exp e = match e with
	ConstExp(_, a) -> a
	| OpExp(_, a) -> a
	| VarExp(_, _, a) -> a
	| TupleExp(_, _, a) -> a
	| NewDimExp(_, _, _, a) -> a
	| AppExp(_, _, a) -> a

let ann_stmt s = match s with
	EvalStmt(_, a) -> a
	| AssignStmt(_, _, a) -> a
	| ReturnStmt(_, a) -> a
	| PatStmt(_, _, a) -> a
	| IfStmt(_, _, _, a) -> a
	| WhileStmt(_, _, a) -> a
	| ForStmt(_, _, _, _, a) -> a

	(*
		######################
		## module declarations
		######################
	*)

type ('m, 'ann) met =
	Method of string * (string * m_type) list * m_type * ('m, 'ann) stmt list

type ('m, 'ann) dec =
	FunDec of ('m, 'ann) met * 'ann

type 'ann req =
	ShortRefReq of string list * 'ann
	| LongRefReq of string list * string list * 'ann

type ('m, 'ann) section =
	Section of 'ann req list * ('m, 'ann) dec list

	(* table of contents *)
	
type n_met = (qual_prefix, l_pos) met
type n_dec = (qual_prefix, l_pos) dec
type n_req = l_pos req
type n_section = (qual_prefix, l_pos) section