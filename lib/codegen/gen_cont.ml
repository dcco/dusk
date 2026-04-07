open Llvm
open Llvm_target

	(* LLVM initialization *)

let context = global_context ();;

install_fatal_error_handler (fun x -> failwith (x ^ "\n"));;
Llvm_all_backends.initialize ();;

	(*
		garbage collector interface
	*)

type gc_iface = {
	new_array: llvalue * lltype;
	gc_alloc: llvalue * lltype;
	gc_new_root: llvalue * lltype;
	gc_collect: llvalue * lltype;
}

	(*
		special LLVM builder context
	*)

type llvm_cont = {
	llmod: llmodule;
	data_layout: DataLayout.t;
	builder: llbuilder;
	gc: gc_iface ref;
	uid: int ref;
}

let newLCont (): llvm_cont =
	let m = create_module context "dusk" in
	let ptrType = pointer_type context in {
		llmod = m;
		data_layout = DataLayout.of_string (data_layout m);
		builder = builder context;
		gc = ref {
			new_array = (const_null ptrType, ptrType);
			gc_alloc = (const_null ptrType, ptrType);
			gc_new_root = (const_null ptrType, ptrType);
			gc_collect = (const_null ptrType, ptrType);
		};
		uid = ref 0;
	}

let genRef (cont: llvm_cont): int = cont.uid := !(cont.uid) + 1; !(cont.uid) - 1

	(*
	let m = create_module context "dusk"
	in LCont(m, DataLayout.of_string (data_layout m), builder context, ref 0)

let setLayoutCont (LCont(m, _, b, r): llvm_cont) (l': DataLayout.t): llvm_cont = LCont(m, l', b, r)

let llmod (LCont(m, _, _, _): llvm_cont): llmodule = m
let builder (LCont(_, _, b, _): llvm_cont): llbuilder = b
let genRef (LCont(_, _, _, r): llvm_cont): int = r := !r + 1; !r - 1
*)

	(*
		dusk compilation type definitions:
			- opaque: opaque array of bytes used for unions, has size + alignment
			- struct: heap-allocated struct, w/ reference to "size" information for GC
	*)

type dusk_tdef =
	OpaqueTD_C of int * int
	| StructTD_C of lltype list * llvalue

	(*
		code generation environment
	*)

type dusk_val = llvalue * lltype

	(*
		- val: normal storage variable (includes alignment when applicable)
	*)

type dusk_fval =
	DVal of dusk_val * int option
	| DGlobal of llvalue
	| DFunVal of llvalue * lltype
	| DTDef of dusk_tdef
	| DEnum of int

type dusk_key =
	DVar of string
	| DStrLit of string
	| DKeyLit of string
	| DBox of int
	| DTName of string
	| DCtor of string
[@@deriving equal, hash]

type dusk_env = (dusk_key, dusk_fval) Hashtbl.t

	(* - TEST fun: used to dump *)

let string_of_dkey (k: dusk_key): string = match k with
	DVar x -> "VAR " ^ x
	| DStrLit s -> "STRLIT " ^ s
	| DKeyLit k -> "KLIT " ^ k
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