open Parser.Dusk_type
open Builtin

let read_subdir_layout (subName: string) (subPath: string): m_virt_bind list =
	Array.to_list (Sys.readdir subPath)
	|> List.map (fun name ->
		if Filename.extension name = ".png" then
			[(QT None, Filename.remove_extension name, ResVD(subName ^ "/" ^ name, imageTy))]
		else []
	)
	|> List.concat

let read_rom_layout (path: string): m_virt_bind list =
	print_string "reading sub-dirs of rom:\n";
	Array.to_list (Sys.readdir path)
	|> List.map (fun subName ->
		let subPath = path ^ "/" ^ subName in
		if Sys.is_directory subPath then read_subdir_layout subName subPath
		else []
	)
	|> List.concat