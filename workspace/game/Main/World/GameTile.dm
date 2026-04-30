
enum BaseType = DeepSea | Sea | Beach | Mud | Low | Mid | Forest | High

struct GameTile{
	BaseType baseType,
	Float elevation,
	Int frontDiff,
	Int leftDiff,
	Int rightDiff
	(*Int elevType,
	Float e1,
	Float e2,
	Float e3,
	Float e4*)
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

const _COLORS = new 1d[
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
	Sulfur.draw(G3QuadY(i, j - tile.elevation, k, tset, tile.baseType.i + 1))
	if tile.frontDiff > 0 then
		Sulfur.draw(G3QuadZ(i, j - tile.elevation, k + 1.0, tset, 0))
	end
	if tile.leftDiff > 0 then
		Sulfur.draw(G3QuadX(i, j - tile.elevation, k, tset, 0))
	end
	if tile.rightDiff > 0 then
		Sulfur.draw(G3QuadX(i + 1.0, j - tile.elevation, k, tset, 0))
	end
end
(*
fn drawId(Int baseType, Int i, Int j)
	Sulfur.draw(GBox(_COLORS[baseType], i, j, 1, 1))
end
*)