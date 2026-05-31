
enum BaseType = DeepSea | Sea | Beach | Mud | Low | Mid | Forest | High | Basement
enum ElevType = FlatElev | HillElev

enum BrickType = Brick | Window | Door1 | Door2

struct GameBrick{
	BrickType type
}

struct GameTile{
	BaseType baseType,
	Int frontDiff,
	Int leftDiff,
	Int rightDiff,
	ElevType elevType,
	Float elevBase,
	Float e1,
	Float e2,
	Float e3,
	Float e4,
	1d[GameBrick] brickList
}

fn isWater(GameTile tile) Bool
	var b = tile.baseType
	if b is DeepSea || b is Sea then return true end
	return false
end

fn canWalk(GameTile tile) Bool
	var b = tile.baseType
	if b is DeepSea || b is Sea || b is High then return false end
	return true
end

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

const _CC = new 1d[
	0xf02020,
	0xe04020,
	0xd08020,
	0x30f020,
	0x20c0e0,
	0x2030f0,
	0x6020c0
]

fn draw(GameTile tile, Float i, Float j, Float k)
	var eType = tile.elevType
	-- draw main tile
	if eType is FlatElev then
		Sulfur.resetFloorSkew()
		Sulfur.drawQuadY(i, j - tile.elevBase, k, tset, tile.baseType.i + 1)
		if tile.frontDiff > 0 then
			Sulfur.drawQuadZ(i, j - tile.elevBase, k + 1.0, tset, 0)
		end
		if tile.leftDiff > 0 then
			Sulfur.drawQuadX(i, j - tile.elevBase, k, tset, 0)
		end
		if tile.rightDiff > 0 then
			Sulfur.drawQuadX(i + 1.0, j - tile.elevBase, k, tset, 0)
		end
	else
		Sulfur.setFloorSkew(-tile.e1, -tile.e2, -tile.e3, -tile.e4)
		Sulfur.drawQuadY(i, j, k, tset, tile.baseType.i + 1)
	end
	-- draw bricks stacked on top when relevant
	if |tile.brickList| > 0 then
		for h < |tile.brickList| do
			var jh = j - tile.elevBase - toFloat(h + 1)
			var bType = tile.brickList[h].type
			if bType is Window then
				Sulfur.drawQuadZ(i, jh, k + 1.0, tset, 23)
			elsif bType is Door1 then
				Sulfur.drawQuadZ(i, jh, k + 1.0, tset, 24)
			elsif bType is Door2 then
				Sulfur.drawQuadZ(i, jh, k + 1.0, tset, 25)
			elsif h = |tile.brickList| - 1 then
				Sulfur.drawQuadX(i, jh, k, tset, 21)
				Sulfur.drawQuadX(i + 1.0, jh, k, tset, 21)
				Sulfur.drawQuadZ(i, jh, k + 1.0, tset, 21)
				Sulfur.drawQuadY(i, jh, k, tset, 22)
			else
				Sulfur.drawQuadX(i, jh, k, tset, 20)
				Sulfur.drawQuadX(i + 1.0, jh, k, tset, 20)
				Sulfur.drawQuadZ(i, jh, k + 1.0, tset, 20)
				Sulfur.drawQuadY(i, jh, k, tset, 20)
			end
		end
	end
end

(*fn drawId(Int baseType, Int i, Int j)
	Sulfur.draw(GBox(_COLORS[baseType], i, j, 1, 1))
end*)

fn drawMini(GameTile tile, Int i, Int j)
	var baseId = tile.baseType.i
	if baseId > |_COLORS| then
		Sulfur.draw(GBox(0x202030, i, j, 1, 1))
	else
		Sulfur.draw(GBox(_COLORS[tile.baseType.i], i, j, 1, 1))
	end
end