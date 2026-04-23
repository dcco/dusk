
struct GameTile{
	Int baseType,
	Float elevation
}

(*
	0 - deep sea
	1 - shallow water

	2 - beach
	3 - mud

	4 - low
	5 - mid
	6 - forest
	7 - high
*)

const colors = new 1d[
	0x485bc4,
	0x5e76ff,
	0xc0b695,
	0xa29877,
	0x859e84,
	0x5c6c5b,
	0x4c654b,
	0x99806c
]

fn draw(GameTile tile, Float i, Float j, Float k)
	Sulfur.draw(G3QuadY(i, j - tile.elevation, k, tset, tile.baseType))
end
(*
fn drawId(Int baseType, Int i, Int j)
	Sulfur.draw(GBox(colors[baseType], i, j, 1, 1))
end
*)