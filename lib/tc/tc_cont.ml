open Resolve.Res_cont

	(*
		typed environment
	*)

type type_env = (string, r_dec) Hashtbl.t