open Yojson.Safe

open Parser.Dusk_type
open Builtin

let read_subdir_layout (subName: string) (subPath: string): m_virt_bind list =
	Array.to_list (Sys.readdir subPath)
	|> List.map (fun name ->
		if Filename.extension name = ".png" then
			let x = Filename.remove_extension name in
			let r = SimpRes("png", x, subName ^ "/" ^ name) in
			[(QT None, x, ResVD(r, imageTy))]
		else []
	)
	|> List.concat

let read_sprite_def (json: Yojson.Safe.t): m_virt_bind = match json with
	`List [`String "TSET"; `String src; `String name; `List [`Int x; `Int y]; `Int sw] ->
		let r = CompRes("sprite", [src], [x; y; sw]) in (QT None, name, ResVD(r, spriteTy))
	| _ -> failwith "TOERR: bad json sprite definition"

let read_rom_layout (path: string): m_virt_bind list =
	let layout = Yojson.Safe.from_file (path ^ "/layout.json") in
	let spriteBinds = Util.member "sprites" layout |> Util.to_list |> List.map read_sprite_def in
	let resBinds = Array.to_list (Sys.readdir path)
	|> List.map (fun subName ->
		let subPath = path ^ "/" ^ subName in
		if Sys.is_directory subPath then read_subdir_layout subName subPath
		else []
	)
	|> List.concat in resBinds @ spriteBinds