open Builtin
open Parser.Dusk_type
open Parser.Dusk_ast
open Codegen.Fin_type

module StringMap = Map.Make(String)

	(*
		tags of types (for overloading)
	*)

let tag_of_type (tau_o: g_type option): string = match tau_o with
	None -> "none"
	| Some (PrimTy x) -> x
	| Some (BuiltinTy x) -> x
	| Some (NamedTy(_, x)) -> x
	| Some (TupleTy tau_l) -> let n = List.length tau_l in
		if n = 2 then "pair"
		else if n = 3 then "triple"
		else "t" ^ (string_of_int n) 
	| Some (ArrayTy(i, _)) -> "a" ^ (string_of_int i)

	(*
		overloaded (polymorphic) function types
	*)

type 'a poly_type = (string * 'a) list

let add_ptype (rho: 'a poly_type) (tag: string) (v: 'a): 'a poly_type =
	match List.find_opt (fun (tag', _) -> tag = tag') rho with
		None -> (tag, v) :: rho
		| Some _ -> failwith "BUG: tc_cont.ml - Attempted to add to polymorphic type with already existing tag."

let disambig_ptype (rho: 'a poly_type) (tag: string): 'a option =
	let rec dp_rec rho = match rho with
		[] -> None
		| (tag', v) :: rho' -> if tag = tag' then Some v else dp_rec rho'
	in dp_rec rho

	(*
		typed environment
	*)

type sym_fun_type = sym * canon_tag fun_type

type poly_dec = sym_fun_type poly_type

type tc_tval =
	TcTDef of g_tdef
	| TcCtor of string

type type_env = {
	globalFIds: (string, poly_dec) Hashtbl.t;
	globalTIds: (string, tc_tval) Hashtbl.t;
	globalIds: (string, g_type) Hashtbl.t;
	localIds: g_type StringMap.t;
	boxCount: int ref
}

	(* - TEST fun: used to dump *)

let dump_tenv (env: type_env): unit =
	print_string "#TYPE_ENV {\n";
	Hashtbl.iter (fun x rho ->
		print_string (" " ^ x ^ ": ");
		if List.length rho = 1 then	print_string (string_of_fun_type (snd (snd (List.hd rho))))
		else (print_string " [\n";
			List.iter (fun (_, (_, tau_f)) -> print_string ("  - " ^ (string_of_fun_type tau_f) ^ "\n")) rho;
			print_string " ]"
		); print_string "\n"
	) env.globalFIds;
	print_string "} {\n";
	Hashtbl.iter (fun x td ->
		print_string (" " ^ x ^ ": "); (match td with
			TcTDef (StructTD _) -> print_string "struct"
			| TcTDef (EnumTD _) -> print_string "enum"
			| TcCtor f -> print_string ("ctor: " ^ f)
		); print_string "\n"
	) env.globalTIds;
	print_string "}\n";;

let add_fun_tenv (env: type_env) (f: string) (v: sym_fun_type): unit =
	let (_, (tau_pl, _)) = v in
	let tag = tag_of_type (hd_opt tau_pl) in match Hashtbl.find_opt env.globalFIds f with
		None -> Hashtbl.add env.globalFIds f [(tag, v)]
		| Some rho -> Hashtbl.replace env.globalFIds f (add_ptype rho tag v) 

let builtin_tenv (dl: g_virt_bind list): type_env =
	let env = {
		globalFIds = Hashtbl.create 50;
		globalTIds = Hashtbl.create 50;
		globalIds = Hashtbl.create 50;
		localIds = StringMap.empty;
		boxCount = ref 0
	} in List.iter (fun (_, f, vd) -> match vd with
		SymVD(s, tau_f) -> add_fun_tenv env f (s, tau_f)
		| ResVD(_, tau) -> Hashtbl.add env.globalIds f tau
		| TDefVD (StructTD fl) ->
			Hashtbl.add env.globalTIds f (TcTDef (StructTD fl))
		| TDefVD (EnumTD cl) ->
			Hashtbl.add env.globalTIds f (TcTDef (EnumTD cl));
			List.iter (fun (c, _, _) -> Hashtbl.add env.globalTIds c (TcCtor f)) cl
	) dl; env

let get_box_id_tenv (env: type_env): int =
	let i = !(env.boxCount) in env.boxCount := i + 1; i

let lookup_fun_tenv (env: type_env) (f: string) (tau_o: g_type option): (string * sym_fun_type) option = match Hashtbl.find_opt env.globalFIds f with
	None -> None (* this case should theoretically never come up *)
	| Some rho ->
		let tag = tag_of_type tau_o in 
		print_string ("TAG+F: " ^ tag ^ " _ " ^ f ^ "\n"); (match disambig_ptype rho tag with
			None -> dump_tenv env; None
			| Some res -> Some ("_" ^ tag ^ f, res)
		)

	(*
		replaces builtin function bindings with type-annotated bindings
	*)

let sym_list_tenv (env: type_env): g_virt_bind list =
	let rho_l = List.of_seq (Hashtbl.to_seq env.globalFIds) in
	List.concat (List.map (fun (f, rho) ->
		List.map (fun (tag, (s, tau_f)) -> (CT, "_" ^ tag ^ f, SymVD(s, tau_f))) rho
	) rho_l)

let tc_complete_builtins (env: type_env) (symList: g_virt_bind list): g_virt_bind list =
	(List.filter (fun (_, _, vd) -> match vd with SymVD(_, _) -> false | _ -> true) symList) @ sym_list_tenv env

	(*
		function context:
			stores important meta-information about the function being compiled
	*)

type fun_cont = {
	f: string;
	lf: lin_flag;
}

let nonLinCont (cont: fun_cont): fun_cont = { cont with lf = Fn }