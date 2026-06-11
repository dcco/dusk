
const _TSIZE = 8

enum RType = NoReify | TopReify | FullReify

enum TType attrs { RType rt, Int sFrame } =
	NullTile{ NoReify, 0 }
	| OldGrass{ TopReify, 0 }
	| BrickGrass{ FullReify, 7 }

struct GameTile{
	TType cType,
	Int rFrame
}

fn draw(GameTile tile, Int x, Int y)
	var tt = tile.cType
	if tt isnt NullTile then
		Sulfur.drawSprite(x, y, tset, tt.sFrame + tile.rFrame, false)
	end
end