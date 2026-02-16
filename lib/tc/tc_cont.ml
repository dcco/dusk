open Builtin
open Parser.Dusk_type
open Resolve.Res_cont
open Codegen.Fin_type

module StringMap = Map.Make(String)

	(*
		tags of types (for overloading)
	*)

let tag_of_type (tau_o: g_type option): string = match tau_o with
	None -> "void"
	| Some (PrimTy x) -> x
	| Some (NamedTy x) -> x
	| Some (TupleTy tau_l) -> let n = List.length tau_l in
		if n = 2 then "pair"
		else if n = 3 then "triple"
		else (string_of_int n) ^ "t" 
	| Some (ArrayTy(i, _)) -> (string_of_int i) ^ "d"

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

type poly_dec = t_sym poly_type

type type_env = {
	global: (string, poly_dec) Hashtbl.t;
	local: g_type StringMap.t
}

	(* - TEST fun: used to dump *)

let dump_tenv (env: type_env): unit =
	print_string "#TYPE_ENV {\n";
	Hashtbl.iter (fun x rho ->
		print_string (x ^ ": ");
		if List.length rho = 1 then	print_string (string_of_fun_type (snd (snd (List.hd rho))))
		else (print_string "[\n";
			List.iter (fun (_, (_, tau_f)) -> print_string ("- " ^ (string_of_fun_type tau_f) ^ "\n")) rho;
			print_string "]"
		); print_string "\n"
	) env.global;
	print_string "}\n";;

let add_fun_tenv (env: type_env) (f: string) (v: t_sym): unit =
	let (_, (tau_pl, _)) = v in
	let tag = tag_of_type (hd_opt tau_pl) in match Hashtbl.find_opt env.global f with
		None -> Hashtbl.add env.global f [(tag, v)]
		| Some rho -> Hashtbl.replace env.global f (add_ptype rho tag v) 

let builtin_tenv (): type_env =
	let env = {
		global = Hashtbl.create 50;
		local = StringMap.empty 
	} in List.iter (fun (f, s, tau_f) -> add_fun_tenv env f (s, tau_f)) (resolve_builtins ()); env

let lookup_fun_tenv (env: type_env) (f: string) (tau_o: g_type option): (string * t_sym) option = match Hashtbl.find_opt env.global f with
	None -> None (* this case should theoretically never come up *)
	| Some rho ->
		let tag = tag_of_type tau_o in (match disambig_ptype rho tag with
			None -> None
			| Some res -> Some ("_" ^ tag ^ f, res)
		)

let sym_list_tenv (env: type_env): (string * t_sym) list =
	let rho_l = List.of_seq (Hashtbl.to_seq env.global) in
	List.concat (List.map (fun (f, rho) ->
		List.map (fun (tag, sym) -> ("_" ^ tag ^ f, sym)) rho
	) rho_l)


