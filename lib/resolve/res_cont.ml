open Commons.Tree_map
open Builtin
open Parser.Lex_token
open Parser.Dusk_type
open Parser.Dusk_ast

module StringMap = Map.Make(String)

	(*
		output of resolution phase
	*)

type bind_origin = PrimOr | LocalOr | ImportOr of string | GlobalOr of string * string

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

type res_binding =
	PrimBind of string
	| SymBind of string
		(* handle, canon global name, symbol *)
	| GlobalSymBind of string * string * string

type res_env = {
	curPath: string list;
	globalModules: ((res_binding list) tree_map) ref;
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
	| GlobalOr(_, tName) -> tName ^ "_" ^ x

let origin_handle (ox: bind_origin): string option = match ox with
	ImportOr handle -> Some handle
	| GlobalOr(handle, _) -> Some handle
	| _ -> None

	(* - standard resolution functions *)

let lookup_env (env: res_env) (p: qual_tag) (x: string): (bind_origin * string) list = match p with
	QT (Some prefix) -> (match Hashtbl.find_opt env.importIds x with
		None -> []
		| Some xl -> List.filter (fun (ox, _) -> origin_handle ox = Some prefix) xl)
	| QT None -> if StringMap.mem x env.localIds then [(PrimOr, x)] else (match Hashtbl.find_opt env.importIds x with
		None -> []
		| Some xl -> xl)

	(* - environment construction functions *)

let valid_path_import_env (env: res_env) (path: string list): bool =
	has_path_tree !(env.globalModules) path

let add_import_env (env: res_env) (path: string list) (handle: string): unit =
	Hashtbl.add env.importPrefixes handle path;
	let symList = lookup_tree !(env.globalModules) path in
	List.iter (fun bind ->
		let (ox, x) = match bind with
			PrimBind x -> (PrimOr, x)
			| SymBind x -> (ImportOr handle, x)
			| GlobalSymBind(h, t, x) -> (GlobalOr(h, t), x)
		in let binding = (ox, x) in match Hashtbl.find_opt env.importIds x with
			None -> Hashtbl.add env.importIds x [binding]
			| Some l -> (match List.find_opt (fun (b, _) -> b = ox) l with
				None -> Hashtbl.replace env.importIds x (binding :: l)
				| _ -> ()
			)
	) symList

	(* - non-overload case: creates locally scoped name, even with conflict *)
let add_bind_dec_env (env: res_env) (ox: bind_origin) (x: string): string =
	(match Hashtbl.find_opt env.importIds x with
		None -> Hashtbl.add env.importIds x [(ox, x)]
		| Some xl -> (match List.find_opt (fun (b, _) -> b = ox) xl with
			None -> Hashtbl.replace env.importIds x ((ox, x) :: xl)
			| _ -> ()
		)
	); canonize_scope env.curPath x;;

	(* - overload case: if conflict, automatically attempts to overload pre-existing name *)
let add_local_dec_env_ol (env: res_env) (x: string): (bind_origin * string) list =
	match Hashtbl.find_opt env.importIds x with
		None -> Hashtbl.add env.importIds x [(LocalOr, x)]; [(LocalOr, x)]
		| Some xl -> xl

let rawBindingWrap ((pf, x): prim_flag * string): res_binding =
	if pf = PF then PrimBind x else SymBind x

let builtin_env (treeMap: (m_virt_bind list) tree_map): res_env = let env = {
	curPath = [];
	globalModules = ref (map_tree (fun vbl -> List.map rawBindingWrap (extractSymbols vbl)) treeMap);
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

	(* - saves all locally declared bindings under a specific import path *)
let extract_local_bindings (importIds: (string, (bind_origin * string) list) Hashtbl.t): res_binding list =
	let rec _find_global_overload ol x = match ol with
		[] -> []
		| (GlobalOr(h, s), _) :: _ -> [(GlobalSymBind(h, s, x))]
		| _ :: t -> _find_global_overload t x
	in Hashtbl.fold (fun x ol bindings ->
		if List.exists (fun (ox, _) -> match ox with
			LocalOr -> true | _ -> false
		) ol then (SymBind x) :: bindings
		else (_find_global_overload ol x) @ bindings
	) importIds []

let save_local_dec_env (env: res_env) (path: string list): unit =
	let bindings = extract_local_bindings env.importIds in
	env.globalModules := add_tree !(env.globalModules) path bindings

let save_ext_dec_env (env: res_env) (path: string list): unit =
	let bindings = extract_local_bindings env.importIds in
		(* TODO: sanity check on extending the bindings here *)
	env.globalModules := update_tree !(env.globalModules) path
		(fun oldBindings -> oldBindings @ bindings) []

	(* - TEST fun: used to dump *)

let dump_renv (env: res_env): unit =
	print_string "#RESOLUTION_ENV {\n";
	dump_tree (fun _ -> "< bindings >") !(env.globalModules);
	Hashtbl.iter (fun x l -> List.iter (fun (ox, _) -> match ox with
		PrimOr -> print_string ("prim: " ^ x ^ "\n")
		| LocalOr -> print_string ("local: " ^ x ^ "\n")
		| ImportOr o -> print_string ("import: " ^ o ^ " - " ^ x ^ "\n")
		| GlobalOr(_, x) -> print_string ("global: " ^ x ^ "\n")
	) l) env.importIds;
	print_string "}\n";;




