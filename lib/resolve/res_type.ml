open Commons.Try_log
open Commons.Tree_map
open Parser.Lex_token
open Parser.Dusk_type
open Builtin
open Codegen.Fin_type
open Res_cont

	(* error types *)

type resolve_err =
	BadLookup_Err of qual_tag * string * l_pos
	| AmbiguousLookup_Err of qual_tag * string * l_pos
	| NonEmptyReqList_Err of l_pos
	| BadReq_Err of string list * l_pos

type 'a rs_res = ('a, resolve_err) try_res

let string_of_name (prefix: qual_tag) (x: string): string = match prefix with
	QT None -> x
	| QT (Some m) -> m ^ "." ^ x

let string_of_rs_err (e: resolve_err) = match e with
	BadLookup_Err(prefix, x, p) -> "Bad lookup of \"" ^ (string_of_name prefix x) ^ "\" at " ^ (string_of_pos p) ^ "."
	| AmbiguousLookup_Err(prefix, x, p) -> "Ambiguous lookup of \"" ^ (string_of_name prefix x) ^ "\" at " ^ (string_of_pos p) ^ "."
	| NonEmptyReqList_Err p -> "Requirement given outside of TOC / top-level at " ^ (string_of_pos p) ^ "."
	| BadReq_Err(xl, p) -> "Bad requirement \"" ^ (String.concat "." xl) ^ "\" at " ^ (string_of_pos p) ^ "."

	(*
		type resolution
	*)

let rec resolve_type (env: res_env) (p: l_pos) (tau: m_type): g_type rs_res = match tau with
	PrimTy x -> Valid (PrimTy x)
	| BuiltinTy x -> Valid (BuiltinTy x)
	| NamedTy(prefix, x) -> (match lookup_env env prefix x with
		[(ox, x')] -> Valid (NamedTy(CT, canonize_binding env ox x'))
		| [] -> Error (BadLookup_Err(prefix, x, p))
		| _ -> Error (AmbiguousLookup_Err(prefix, x, p))
	)
	| TupleTy tau_l -> let* tau_l' = map_try_res (resolve_type env p) tau_l in Valid (TupleTy tau_l')
	| ArrayTy(i, tau) -> let* tau' = resolve_type env p tau in Valid (ArrayTy(i, tau'))
	| ValArrayTy tau -> let* tau' = resolve_type env p tau in Valid (ValArrayTy tau')

let resolve_type_def (env: res_env) (p: l_pos) (td: m_tdef): g_tdef rs_res = match td with
	StructTD fl ->
		let* fl' = map_try_res (fun (x, tau) ->
			let* tau' = resolve_type env p tau in Valid (x, tau')
		) fl in Valid (StructTD fl')
	| EnumTD cl ->
		let prefix = QT None in
		let* cl' = map_try_res (fun (x, tau_l, ext) ->
			let* tau_l' = map_try_res (resolve_type env p) tau_l in (match lookup_env env prefix x with
				[(ox, x')] -> Valid (canonize_binding env ox x', tau_l', ext)
				| [] -> Error (BadLookup_Err(prefix, x, p))
				| _ -> Error (AmbiguousLookup_Err(prefix, x, p))
			)
		) cl in Valid (EnumTD cl')

	(* 
		builtin resolution
	*)

let resolve_virt_dec (env: res_env) (vd: qual_tag virt_dec): canon_tag virt_dec =
	let vd_res = (match vd with
		SymVD(s, (tau_pl, tau_r)) ->
			let* tau_pl' = map_try_res (resolve_type env Lexing.dummy_pos) tau_pl in
			let* tau_r' = resolve_type env Lexing.dummy_pos tau_r in Valid (SymVD(s, (tau_pl', tau_r')))
		| ResVD(url, tau) ->
			let* tau' = resolve_type env Lexing.dummy_pos tau in Valid (ResVD(url, tau'))
		| TDefVD td ->
			let* td' = resolve_type_def env Lexing.dummy_pos td in
			Valid (TDefVD td')
	) in (match vd_res with
		Valid vd' -> vd'
		| Error e ->
			print_string ((string_of_rs_err e) ^ "\n");
			failwith "BUG: res_type.ml - Error resolving type in virtual declaration."
	)

let resolve_virt_bindings (env: res_env) (bindings: (string list * m_virt_bind) list): g_virt_bind list =
		(* add all builtin symbols to environment with default import strategy *)
	let env' = freeze_env env [] in
	let pl = paths_tree !(env.globalModules) in
	List.iter (fun path ->
		let i = List.length path - 1 in
		add_import_env env' path (List.nth path i)
	) pl;
		(* canonize virtual declarations (especially types / typedefs) *)
	List.map (fun (path, (_, x, vd)) ->
		let x' = (match vd with TDefVD _ -> x | _ -> canonize_scope path x) in
		(CT, x', resolve_virt_dec env' vd)
	) bindings
