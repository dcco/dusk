open Commons.Tree_map
open Builtin
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

type r_section = SectionR of (string * r_dec) list

	(*
		temporary environment
		* global - all bindings w/ module structure preserved
		* importHeader -  maps import prefixes -> full path names
		* import - bindings imported locally to the module
		-- mapping includes import prefix + original name (applicable when aliased)
		* local - bindings declared locally in a function
	*)

type global_env = (string list) tree_map
type import_env = (string, (bind_origin * string) list) Hashtbl.t
type local_env = (string, unit) Hashtbl.t

type res_env = {
	global: global_env;
	importHeader: (string, string list) Hashtbl.t;
	import: import_env;
	local: local_env;
}

let add_import_env (env: res_env) (path: string list) (handle: string): unit =
	Hashtbl.add env.importHeader handle path;
	let symList = lookup_tree env.global path in
	List.iter (fun x ->
		let binding = (ImportOr handle, x) in
		match Hashtbl.find_opt env.import x with
			None -> Hashtbl.add env.import x [binding]
			| Some l -> Hashtbl.replace env.import x (binding :: l)
	) symList

let builtin_env (): res_env = let env = {
	global = map_tree (fun l -> List.map (fun (f, _, _) -> f) l) (builtinTreeMap ());
	importHeader = Hashtbl.create 1;
	import = Hashtbl.create 1;
	local = Hashtbl.create 1
} in add_import_env env ["Predef"] ""; env

let freeze_env (env: res_env): res_env = {
	global = env.global;
	importHeader = Hashtbl.create 5;
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

	(* canonization functions *)

let canonize_scope (scope: string list) (x: string): string =
	"_" ^ (String.concat "_" scope) ^ "_" ^ x

let canonize_binding (env: res_env) (ox: bind_origin) (x: string): string = match ox with
	LocalOr -> x
	| ImportOr handle -> (match Hashtbl.find_opt env.importHeader handle with
		None -> failwith "BUG: res_cont.ml - Attempted to lookup unknown import handle."
		| Some path -> canonize_scope path x
	)

	(* prepare builtins for resolution *)

let resolve_builtins (): virt_dec list =
	List.map (fun (path, (x, s, tau_f)) -> (canonize_scope path x, s, tau_f)) (builtinQualList ())


	(*
		final environment
		- flattens the environment created during resolution
	*)
(*
type full_env = (string, r_dec) Hashtbl.t

let resolve_res_env (env: res_env): full_env =
	let symTable = flatten_tree env.global in
	let bindingList = List.concat (List.map (fun (path, symList) ->
		let prefix = (String.concat "_" path) ^ "_" in
		List.map (fun (f, d) -> (prefix ^ f, d)) symList	
	) symTable) in
	let table = Hashtbl.create (List.length bindingList) in
	List.iter (fun (k, v) -> Hashtbl.add table k v) bindingList; table*)


(*


	(*
		name canonization functions
	*)

let canonize_scope (scope: string list) (x: string): string =
	"_" ^ (String.concat "_" scope) ^ "_" ^ x

let canonize_scope_tag (scope: string list) (tag: string) (x: string): string =
	"_" ^ (String.concat "_" scope) ^ "_" ^ tag ^ "_" ^ x
*)