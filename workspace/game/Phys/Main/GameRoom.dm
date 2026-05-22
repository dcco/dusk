
struct GameRoom{
	Int width,
	Int height,
	2d[GameTile] grid,
	1d[MoveObj] objList
}

	(* read functions *)

fn elevation(GameRoom room, Int cx, Int cy) Float
	if cx < 0 || cx >= room.width || cy < 0 || cy >= room.height then return 0.0 end
	return room.grid[cx, cy].elevBase
end

	(* simple modifier functions *)

fn addObj(GameRoom room, MoveObj obj, Int px, Int pz)
	room.objList += obj
	obj.hitbox.x = fromPixel(px)
	obj.hitbox.z = fromPixel(pz)
	var cx = toTile(obj.hitbox.centerX())
	var cz = toTile(obj.hitbox.centerZ())
	obj.hitbox.y = -fromFP(room.elevation(cx, cz)) - obj.hitbox.height
end

	(*
		full update function
	*)

fn updateObj(GameRoom room, MoveObj obj)
	obj.hitbox.x = obj.hitbox.x + obj.xspd
	obj.hitbox.z = obj.hitbox.z + obj.zspd
	var cx = toTile(obj.hitbox.centerX())
	var cz = toTile(obj.hitbox.centerZ())
	obj.hitbox.y = -fromFP(room.elevation(cx, cz)) - obj.hitbox.height
end

fn update(GameRoom room)
	for i < |room.objList| do
		room.updateObj(room.objList[i])
	end
end

	(*
		draw function
	*)

fn draw(GameRoom room, Int cx, Int cy)
	for i < 30, j < 30 do
		var tx = cx + i - 15
		var ty = cy + j - 20
		if tx >= 0 && tx < room.width && ty >= 0 && ty < room.height then
			room.grid[tx, ty].draw(toFloat(tx), 0.0, toFloat(ty))
		end
	end
	Sulfur.setAttr(0, GLFloat4(0.0, 0.0, 0.0, 0.0))
end