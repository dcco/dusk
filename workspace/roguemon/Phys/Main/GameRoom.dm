
struct GameRoom{
	Int width,
	Int height,
	2d[GameTile] grid,
	1d[MoveObj] objList
}

	(* read functions *)

fn elevation(GameRoom room, Int cx, Int cy) Float
	if cx < 0 || cx >= room.width || cy < 0 || cy >= room.height then return 0.0 end
	return room.grid[cx, cy].elevBase + toFloat(|room.grid[cx, cy].brickList|)
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

fn pushTile(GameRoom room, (Int, Int) moveRange, Int axis, Int delta, Int tx, Int tz) (Int, Int)
	if axis = 0 then
		var tileRange = (fromTile(tx), fromTile(tx + 1))
		return moveRange.diff(tileRange, delta)
	else
		var tileRange = (fromTile(tz), fromTile(tz + 1))
		return moveRange.diff(tileRange, delta)
	end
end

fn precalcObj(GameRoom room, MoveObj obj, Int axis, Int delta) Int
	var hitbox = obj.hitbox
	var moveRange = hitbox.rangeI(axis).extend(delta)
	var moveBox = hitbox.chxRangeH(axis, moveRange)
	var chkLeft = toTile(moveBox.1)
	var chkRight = toTile(moveBox.2 - 1)
	var chkTop = toTile(moveBox.3)
	var chkBot = toTile(moveBox.4 - 1)
	for i <= chkRight - chkLeft, j <= chkBot - chkTop do
		var tx = chkLeft + i
		var tz = chkTop + j
		if -fromFP(room.elevation(tx, tz)) < obj.hitbox.bottom() - fromPixel(3) then
			moveRange = room.pushTile(moveRange, axis, delta, tx, tz)
		end
	end
	if delta > 0 then return moveRange.2 - moveRange.1
	else return moveRange.1 - moveRange.2 end
end

fn moveObjH(GameRoom room, MoveObj obj, Int axis, Int delta) Int
	var newDelta = room.precalcObj(obj, axis, delta)
	if axis = 0 then obj.hitbox.x = obj.hitbox.x + newDelta
	else obj.hitbox.z = obj.hitbox.z + newDelta end
	return newDelta
end

fn updateObj(GameRoom room, MoveObj obj)
	-- update speed
	obj.updateSpd()
	obj.standFlag = false
	-- move horizontal
	room.moveObjH(obj, 0, obj.xspd)
	room.moveObjH(obj, 2, obj.zspd)
	-- move vertical
	var cx = toTile(obj.hitbox.centerX())
	var cz = toTile(obj.hitbox.centerZ())
	var maxY = -fromFP(room.elevation(cx, cz)) - obj.hitbox.height
	obj.hitbox.y = obj.hitbox.y + obj.yspd
	if obj.hitbox.y > maxY then
		var deltaY = (obj.hitbox.y - maxY) / 3
		if deltaY < fromPixel(1) then
			obj.hitbox.y = maxY
		else
			obj.hitbox.y = obj.hitbox.y - deltaY
		end
		obj.yspd = 0
	end
	if obj.hitbox.y >= maxY then
		obj.standFlag = true
	end
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
	Sulfur.resetFloorSkew()
end

fn drawMini1(GameRoom room, Int cx, Int cy)
	for i < 120, j < 120 do
		var tx = cx + i - 60
		var ty = cy + j - 60
		if tx < 0 || tx >= room.width || ty < 0 || ty >= room.height then
			Sulfur.draw(GBox(_COLORS[0], i, j, 1, 1))
		else
			room.grid[tx, ty].drawMini(i, j)
		end
	end
end

fn drawMini2(GameRoom room)
	for i < room.width / 5, j < room.height / 5 do
		var tx = i * 5
		var ty = j * 5
		room.grid[tx, ty].drawMini(i, j)
	end
end

fn drawMini(GameRoom room, Bool miniMode, Int cx, Int cy)
	if !miniMode then
		room.drawMini1(cx, cy)
	else
		room.drawMini2()
	end
end