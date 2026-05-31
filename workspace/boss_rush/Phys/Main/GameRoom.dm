
struct GameRoom{
	Int width,
	Int height,
	2d[GameTile] data,
	1d[MoveObj] objList
}

fn newRoom(Int w, Int h) GameRoom
	var data = new 2d(w, h)[.. new GameTile{ type = Empty }]
	return new GameRoom{
		width = w,
		height = h,
		data = data,
		objList = new 1d[~MoveObj]
	}
end

fn contains(GameRoom room, Int i, Int j) Bool
	return i >= 0 && i < room.width && j >= 0 && j < room.height
end

fn solid(GameRoom room, Int i, Int j) Bool
	if !room.contains(i, j) then return true end
	var t = room.data[i, j].type
	return !t is Empty
end

fn addObj(GameRoom room, MoveObj obj)
	room.objList += obj
end

	(*
		wall check functions
	*)

fn updateWall(GameRoom room, MoveObj obj)
	var wall = obj.wall
	-- horiz checks
	for dir < 2 do
		var chkX = 0
		if dir = 0 then chkX = obj.hitbox.x - 1
		else chkX = obj.hitbox.right() end
		var chkTop = toTile(obj.hitbox.y)
		var chkBot = toTile(obj.hitbox.bottom())
		for j <= chkBot - chkTop do
			var ty = chkTop + j
			if room.solid(toTile(chkX), ty) then
				if dir = 0 then wall.left = true
				else wall.right = true end
			end
		end
	end
	-- vert checks
	for dir < 2 do
		var chkY = 0
		if dir = 0 then chkY = obj.hitbox.y - 1
		else chkY = obj.hitbox.bottom() end
		var chkLeft = toTile(obj.hitbox.x)
		var chkRight = toTile(obj.hitbox.right())
		for i <= chkRight - chkLeft do
			var tx = chkLeft + i
			if room.solid(tx, toTile(chkY)) then
				if dir = 0 then wall.top = true
				else wall.bot = true end
			end
		end
	end
end

	(*
		main collision / movement functions
	*)

fn pushTile(GameRoom room, (Int, Int) moveRange, Int axis, Int delta, Int tx, Int ty) (Int, Int)
	if axis = 0 then
		var tileRange = (fromTile(tx), fromTile(tx + 1))
		return moveRange.diff(tileRange, delta)
	else
		var tileRange = (fromTile(ty), fromTile(ty + 1))
		return moveRange.diff(tileRange, delta)
	end
end

fn precalcObj(GameRoom room, MoveObj obj, Int axis, Int delta) Int
	var hitbox = obj.hitbox
	var moveRange = hitbox.rangeI(axis).extend(delta)
	var moveBox = hitbox.chxRange(axis, moveRange)
	var chkLeft = toTile(moveBox.1)
	var chkRight = toTile(moveBox.2 - 1)
	var chkTop = toTile(moveBox.3)
	var chkBot = toTile(moveBox.4 - 1)
	for i <= chkRight - chkLeft, j <= chkBot - chkTop do
		var tx = chkLeft + i
		var ty = chkTop + j
		if room.solid(tx, ty) then
			moveRange = room.pushTile(moveRange, axis, delta, tx, ty)
		end
	end
	if delta > 0 then return moveRange.2 - moveRange.1
	else return moveRange.1 - moveRange.2 end
end

fn moveObj(GameRoom room, MoveObj obj, Int axis, Int delta) Int
	var newDelta = room.precalcObj(obj, axis, delta)
	--room.moveObjDelta(obj, axis, delta)
	if axis = 0 then obj.hitbox.x = obj.hitbox.x + newDelta
	else obj.hitbox.y = obj.hitbox.y + newDelta end
	return newDelta
end

fn updateObj(GameRoom room, MoveObj obj)
	-- update speed
	obj.updateSpd()
	-- reset wall flags
	obj.wall.reset()
	var oldY = obj.hitbox.y
	-- move
	var deltaX = room.moveObj(obj, 0, obj.xspd)
	var deltaY = room.moveObj(obj, 1, obj.yspd)
	obj.xspd = deltaX
	obj.yspd = deltaY
	room.updateWall(obj)
end

	(*
		full update function
	*)

fn update(GameRoom room)
	for i < |room.objList| do
		room.updateObj(room.objList[i])
	end
end

	(*
		render functions
	*)

fn draw(GameRoom room)
	for i < room.width, j < room.height do
		var tt = room.data[i, j].type
		if tt is PGrass then
			Sulfur.drawQuadZ(toFloat(i), toFloat(j), 0.0, tset, 5)
		end
	end
end