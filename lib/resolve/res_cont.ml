open Commons.Tree_map
open Parser.Lex_token
open Parser.Dusk_ast

	(*
		output of resolution phase
	*)

type bind_origin = LocalOr | ImportOr of string

type r_exp = (unit, l_pos) exp
type r_stmt = (unit, l_pos) stmt
type r_met = (unit, l_pos) met
type r_dec = (unit, l_pos) dec
type r_section = (unit, l_pos) section

	(*
		temporary environment
		* global - canon AST w/ module structure preserved
		* import - bindings imported locally to the module
		-- mapping includes import prefix + name
		* local - bindings declared locally in a function
	*)

type global_env = r_section tree_map
type import_env = (string, (bind_origin * string) list) Hashtbl.t
type local_env = (string, unit) Hashtbl.t

type res_env = {
	global: global_env;
	import: import_env;
	local: local_env;
}

let empty_env (): res_env = {
	global = empty_tree ();
	import = Hashtbl.create 20;
	local = Hashtbl.create 20
}

let lookup_env (env: res_env) (p: string option) (x: string): (bind_origin * string) list = match p with
	None -> if Hashtbl.mem env.local x then [(LocalOr, x)] else (match Hashtbl.find_opt env.import x with
		None -> []
		| Some xl -> xl)
	| Some prefix -> (match Hashtbl.find_opt env.import x with
		None -> []
		| Some xl -> List.filter (fun (ox, _) -> ox = ImportOr prefix) xl
	)