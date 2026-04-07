open Commons.Tree_map
open Builtin
open Parser.Lex_token
open Parser.Dusk_type
open Parser.Dusk_ast

module StringMap = Map.Make(String)

	(*
		output of resolution phase
	*)

type bind_origin = PrimOr | LocalOr | ImportOr of string

type r_exp = (canon_tag, l_pos) exp
type r_stmt = (canon_tag, l_pos) stmt
type r_met = (canon_tag, l_pos) met
type r_dec = (canon_tag, l_pos) dec

type r_section = SectionR of r_dec list

	(*
		temporary environment
		* globalModules - maps module paths -> binding lists (bindings w/ module structure preserved)
		* importPrefixes - maps import prefixes -> full path names
		* importIds - (all) bindings imported locally to the module / declared at module level
		-- mapping includes import prefix + original name (applicable when aliased)
		* localIds - (values) bindings declared locally in a function

		primitive flag - flag indicating that the canonical name should be unqualified 
	*)

type res_env = {
	curPath: string list;
	globalModules: ((prim_flag * string) list) tree_map;
	importPrefixes: (string, string list) Hashtbl.t;
	importIds: (string, (bind_origin * string) list) Hashtbl.t;
	localIds: unit StringMap.t;
}

	(*
		canonization functions
	*)

let canonize_scope (scope: string list) (x: string): string =
	if List.length scope = 0 then "_" ^ x
	else "_" ^ (String.concat "_" scope) ^ "_" ^ x

let canonize_binding (env: res_env) (ox: bind_origin) (x: string): string = match ox with
	PrimOr -> x
	| LocalOr -> canonize_scope env.curPath x
	| ImportOr handle -> (match Hashtbl.find_opt env.importPrefixes handle with
		None -> failwith "BUG: res_cont.ml - Attempted to lookup unknown import handle."
		| Some path -> canonize_scope path x
	)

	(* - standard resolution functions *)

let lookup_env (env: res_env) (p: qual_tag) (x: string): (bind_origin * string) list = match p with
	QT (Some prefix) -> (match Hashtbl.find_opt env.importIds x with
		None -> []
		| Some xl -> List.filter (fun (ox, _) -> ox = ImportOr prefix) xl)
	| QT None -> if StringMap.mem x env.localIds then [(PrimOr, x)] else (match Hashtbl.find_opt env.importIds x with
		None -> []
		| Some xl -> xl)

	(* - environment construction functions *)

let add_import_env (env: res_env) (path: string list) (handle: string): unit =
	Hashtbl.add env.importPrefixes handle path;
	let symList = lookup_tree env.globalModules path in
	List.iter (fun (pf, x) ->
		let ox = (match pf with PF -> PrimOr | _ -> ImportOr handle) in
		let binding = (ox, x) in
		match Hashtbl.find_opt env.importIds x with
			None -> Hashtbl.add env.importIds x [binding]
			| Some l -> (match List.find_opt (fun (b, _) -> b = ox) l with
				None -> Hashtbl.replace env.importIds x (binding :: l)
				| _ -> ()
			)
	) symList

	(* - non-overload case: creates locally scoped name, even with conflict *)
let add_local_dec_env (env: res_env) (x: string): string =
	(match Hashtbl.find_opt env.importIds x with
		None -> Hashtbl.add env.importIds x [(LocalOr, x)]
		| Some xl -> (match List.find_opt (fun (b, _) -> b = LocalOr) xl with
			None -> Hashtbl.replace env.importIds x ((LocalOr, x) :: xl)
			| _ -> ()
		)
	); canonize_scope env.curPath x;;

	(* - overload case: if conflict, automatically attempts to overload pre-existing name *)
let add_local_dec_env_ol (env: res_env) (x: string): (bind_origin * string) list =
	match Hashtbl.find_opt env.importIds x with
		None -> Hashtbl.add env.importIds x [(LocalOr, x)]; [(LocalOr, x)]
		| Some xl -> xl

let builtin_env (treeMap: (m_virt_bind list) tree_map): res_env = let env = {
	curPath = [];
	globalModules = map_tree extractSymbols treeMap;
	importPrefixes = Hashtbl.create 5;
	importIds = Hashtbl.create 20;
	localIds = StringMap.empty
} in add_import_env env ["builtin"] ""; env

let freeze_env (env: res_env) (path: string list): res_env = let env = {
	curPath = path;
	globalModules = env.globalModules;
	importPrefixes = Hashtbl.create 5;
	importIds = Hashtbl.create 20;
	localIds = StringMap.empty
} in add_import_env env ["builtin"] ""; env

	(* - TEST fun: used to dump *)

let dump_renv (env: res_env): unit =
	print_string "#RESOLUTION_ENV {\n";
	Hashtbl.iter (fun x l -> List.iter (fun (ox, _) -> match ox with
		PrimOr -> print_string ("prim: " ^ x ^ "\n")
		| LocalOr -> print_string ("local: " ^ x ^ "\n")
		| ImportOr o -> print_string ("import: " ^ o ^ " - " ^ x ^ "\n")
	) l) env.importIds;
	print_string "}\n";;




