module StringMap = Map.Make(String)

	(*
		tree_map: tree data structure for information stored along paths
	*)

type 'a tree_map =
	Leaf of 'a
	| Branch of 'a tree_map StringMap.t

	(* - constructors *)

let empty_tree (): 'a tree_map = Branch StringMap.empty

let rec single_tree (path: string list) (v: 'a): 'a tree_map = match path with
	[] -> Leaf v
	| x :: xt -> Branch (StringMap.singleton x (single_tree xt v))

	(* - lookup / search *)

let rec open_path_tree (tree: 'a tree_map) (path: string list): bool = match (path, tree) with
	(_, Leaf _) -> false
	| ([], Branch _) -> false
	| (x :: xt, Branch childMap) -> (match StringMap.find_opt x childMap with
		None -> true
		| Some child -> open_path_tree child xt
	)

let rec has_path_tree (tree: 'a tree_map) (path: string list): bool = match (path, tree) with
	([], Leaf _) -> true
	| (x :: xt, Branch childMap) -> (match StringMap.find_opt x childMap with
		None -> false
		| Some child -> has_path_tree child xt
	)
	| _ -> false

let rec lookup_tree (tree: 'a tree_map) (path: string list): 'a = match (path, tree) with
	([], Leaf v) -> v
	| (x :: xt, Branch childMap) ->	lookup_tree (StringMap.find x childMap) xt
	| _ -> raise Not_found

	(* - insertion *)

let rec add_tree (tree: 'a tree_map) (path: string list) (v: 'a): 'a tree_map = match (path, tree) with
	(_, Leaf _) -> failwith "tree_map.ml - Leaf found on path while adding path to tree."
	| ([], Branch _) -> failwith "tree_map.ml - Branch found at end of path while adding path to tree."
	| (x :: xt, Branch childMap) ->
		Branch (StringMap.update x (fun l -> match l with
			None -> Some (single_tree xt v)
			| Some child -> Some (add_tree child xt v)
		) childMap)

	(* - FUTURE: update / removal *)

	(* - conversion *)

let rec map_tree (f: 'a -> 'b) (tree: 'a tree_map): 'b tree_map = match tree with
	Leaf v -> Leaf (f v)
	| Branch m -> Branch (StringMap.map (map_tree f) m)

let flatten_tree (tree: 'a tree_map): (string list * 'a) list =
	let rec ft_rec tree path = match tree with
		Leaf v -> [(path, v)]
		| Branch childMap -> List.concat (List.map (fun (k, child) ->
			ft_rec child (path @ [k])
		) (StringMap.bindings childMap))
	in ft_rec tree []