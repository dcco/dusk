
let (let>) (x: 'a option) (f: 'a -> 'b option): 'b option =
	match x with None -> None | Some v -> f v

	(* generic type for functions / calculations that may return an error result *)

type ('a, 'b) try_res =
	Valid of 'a
	| Error of 'b

let (let*) (x: ('a, 'c) try_res) (f: 'a -> ('b, 'c) try_res): ('b, 'c) try_res =
	match x with Error e -> Error e | Valid v -> f v

	(* type for (possible) error results + logging state *)

type 'a try_log_res = LogRes of 'a option * string list

let tryWithErrLog (f: 'b -> string) (x: ('a, 'b) try_res): 'a try_log_res =
	match x with Error e -> LogRes(None, [f e]) | Valid v -> LogRes(Some v, [])

let (let*!) (x: 'a try_log_res) (f: 'a -> 'b try_log_res): 'b try_log_res = match x with
	LogRes(Some v, lx) -> let LogRes(r, ly) = f v in LogRes(r, lx @ ly)
	| LogRes(None, lx) -> LogRes(None, lx)

let validLog (v: 'a): 'a try_log_res = LogRes(Some v, [])

let failLog (x: string): 'a try_log_res = LogRes(None, [x])

let endLog (): 'a try_log_res = LogRes(None, [])

let printLogRes (LogRes(_, lx): 'a try_log_res) =
	List.map (fun x -> print_string (x ^ "\n")) lx

let hookLogRes (err: string -> unit) (LogRes(v, lx): 'a try_log_res): unit = match v with
	None -> err (String.concat "\n" lx)
	| _ -> ()