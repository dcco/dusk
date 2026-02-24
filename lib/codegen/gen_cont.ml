open Llvm
open Llvm_target

	(* LLVM initialization *)

let context = global_context ();;

install_fatal_error_handler (fun x -> failwith (x ^ "\n"));;
Llvm_all_backends.initialize ();;

	(*
		special LLVM builder context
	*)

type llvm_cont = LCont of llmodule * DataLayout.t * llbuilder * int ref

let newLCont (): llvm_cont =
	let m = create_module context "dusk"
	in LCont(m, DataLayout.of_string (data_layout m), builder context, ref 0)

let llmod (LCont(m, _, _, _): llvm_cont): llmodule = m
let builder (LCont(_, _, b, _): llvm_cont): llbuilder = b
let genRef (LCont(_, _, _, r): llvm_cont): int = r := !r + 1; !r - 1

	(*
		dusk compilation type definitions
	*)

type dusk_tdef =
	OpaqueTD_C of int

	(*
		code generation environment
	*)

type dusk_val = llvalue * lltype

type dusk_fval =
	DVal of dusk_val
	| DGlobal of llvalue
	| DFunVal of llvalue * lltype
	| DTDef of dusk_tdef
	| DEnum of int

type dusk_key =
	DVar of string
	| DStrLit of string
	| DBox of int
	| DTName of string
	| DCtor of string
[@@deriving equal, hash]

type dusk_env = (dusk_key, dusk_fval) Hashtbl.t

	(* - TEST fun: used to dump *)

let string_of_dkey (k: dusk_key): string = match k with
	DVar x -> "VAR " ^ x
	| DStrLit s -> "STRLIT " ^ s
	| DBox i -> "BOX " ^ (string_of_int i)
	| DTName t -> "TNAME " ^ t
	| DCtor c -> "CTOR " ^ c

let dump_denv (env: dusk_env): unit =
	print_string "#CODEGEN_ENV {\n";
	Hashtbl.iter (fun k _ ->
		print_string ((string_of_dkey k) ^ ": ");
		print_string "\n"
	) env;
	print_string "}\n";;