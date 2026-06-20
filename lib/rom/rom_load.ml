open Yojson.Safe

open Builtin

let read_subdir_layout (fontInfo: (string, int list) Hashtbl.t) (subName: string) (subPath: string): m_virt_bind list =
	Array.to_list (Sys.readdir subPath)
	|> List.map (fun name ->
		if Filename.extension name = ".png" then
			let x = Filename.remove_extension name in
			let r = SimpRes("png", subName, x, subName ^ "/" ^ name, []) in
			[(rb x, ResVD(SRD r, imageTy))]
		else if Filename.extension name = ".ttf" then
			let x = Filename.remove_extension name in
			let il = (match Hashtbl.find_opt fontInfo x with
				Some il -> il
				| _ -> failwith ("TOERR: font " ^ x ^ " loaded with no font specification in layout.")
			) in
			let r = SimpRes("ttf", subName, x, subName ^ "/" ^ name, il) in
			[(rb x, ResVD(SRD r, fontTy))]
		else []
	)
	|> List.concat

let read_sprite_def (json: Yojson.Safe.t): m_virt_bind = match json with
	`List [`String "TSET"; `String src; `String name; `List [`Int x; `Int y]; `Int sw] ->
		let r = CompRes("sprite", [src], [x; y; sw; 1; 1]) in (rb name, ResVD(CRD r, spriteTy))
	| `List [`String "SPRITE"; `String src; `String name; `List [`Int x; `Int y]; `List [`Int fx; `Int fy]; `Int sw] ->
		let r = CompRes("sprite", [src], [x; y; sw; fx; fy]) in (rb name, ResVD(CRD r, spriteTy))
	| _ -> failwith "TOERR: bad json sprite definition"

let read_font_def (json: Yojson.Safe.t): (string * int list) = match json with 
	`List [`String name; `List sizeList] -> (name, List.map (fun sizeJson -> match sizeJson with
		`Int i -> i
		| _ -> failwith "TOERR: bad json font size value"
	) sizeList)
	| _ -> failwith "TOERR: bad json font definition"

let read_rom_layout (path: string): m_virt_bind list =
	if not (Sys.file_exists (path ^ "/layout.json")) then []
	else let layout = Yojson.Safe.from_file (path ^ "/layout.json") in
	let spriteBinds = Util.member "sprites" layout |> Util.to_list |> List.map read_sprite_def in
	let fontBinds = Util.member "fonts" layout |> Util.to_list |> List.map read_font_def in
	let fontInfo = Hashtbl.create (List.length fontBinds) in
	List.iter (fun (f, il) -> Hashtbl.add fontInfo f il) fontBinds;
	let resBinds = Array.to_list (Sys.readdir path)
	|> List.map (fun subName ->
		let subPath = path ^ "/" ^ subName in
		if Sys.is_directory subPath then read_subdir_layout fontInfo subName subPath
		else []
	)
	|> List.concat in resBinds @ spriteBinds