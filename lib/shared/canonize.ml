
	(*
		name canonization functions
	*)

let canonize_scope (scope: string list) (x: string): string =
	"_" ^ (String.concat "_" scope) ^ "_" ^ x

let canonize_scope_tag (scope: string list) (tag: string) (x: string): string =
	"_" ^ (String.concat "_" scope) ^ "_" ^ tag ^ "_" ^ x