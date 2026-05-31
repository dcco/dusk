
enum WMode = GameMode | BattleMode

enum WFlag = NoFlag | BattleFlag

struct WorldFlag{
	WFlag state,
	1d[GameObj] args
}

struct Targeter{
	1d[GameObj] obj
}

struct GameWorld{
	GameRoom room,
	1d[GameObj] objList,	
	GameObj player,
	Targeter target,
	Camera camera,
	WMode mode,
	WorldFlag flag,
--	1d[CmdW] cmdList,

	Bool miniMode,
	Int sectorX,
	Int sectorY,
	Int curSX,
	Int curSY
	(*Float px,
	Float pz*)
}

fn loadSector(GameWorld world, Int sx, Int sy)
	for i < 30, j < 30 do
		var tx = sx * 30 + i
		var ty = sy * 30 + j
		var tile = world.room.grid[tx, ty]
		if Os.randomInt(150) = 0 && !tile.isWater() then
			var t = CritMon
			if Os.randomInt(5) < 2 then t = CrabMon end
			var newObj = newGameObj(t)
			world.room.addObj(newObj.body, tilePixel(tx), tilePixel(ty))
			world.objList += newObj
		end
	end
end

fn unloadSectors(GameWorld world, Int sx, Int sy)
	var newList = new 1d[~GameObj]
	for i < |world.objList| do
		var curObj = world.objList[i]
		var type = curObj.type
		var osx = toTile(curObj.body.hitbox.centerX()) / 30
		var osy = toTile(curObj.body.hitbox.centerZ()) / 30
		if type is Player || (osx >= sx && osx <= sx + 3 && osy >= sy && osy <= sy + 3) then
			newList += curObj
		end
	end
	world.objList = newList
end

fn reloadSectors(GameWorld world, Int sx, Int sy)
	-- unload sectors outside of the range
	world.unloadSectors(sx, sy)
	-- load sectors inside new range
	for i < 4, j < 4 do
		var si = sx + i
		var sj = sy + j
		if si < world.sectorX || si > world.sectorX + 3 || sj < world.sectorY || sj > world.sectorY + 3 then
			world.loadSector(sx + i, sy + j)
		end
	end
	world.sectorX = sx
	world.sectorY = sy
end

fn newWorld() GameWorld
	-- initialize room
	var (room, startX, startY) = generateRoom()
	-- initialize player
	var player = newGameObj(Player)
	var initList = new 1d[~GameObj]
	room.addObj(player.body, tilePixel(startX), tilePixel(startY))
	initList += player
	-- initial object spawn
	var sx = toTile(player.body.hitbox.centerX()) / 30
	var sy = toTile(player.body.hitbox.centerZ()) / 30
	-- final return
	var world = new GameWorld{
		room = room,
		objList = initList,
		player = player,
		target = new Targeter{
			obj = new 1d[~GameObj],
			aimFlag = false
		},
		camera = newCamera(player.body),
		flag = new WorldFlag{
			state = NoFlag,
			args = new 1d[~GameObj]
		},
		mode = GameMode,
		miniMode = false,
		sectorX = -99,
		sectorY = -99,
		curSX = sx,
		curSY = sy
	}
	world.reloadSectors(sx - 1, sy - 1)
	return world
end

--const _SPEED = 0x150
const _SPEED = 0x200

fn pcInput(GameWorld world)
	-- player update
	var player = world.player
	var hdX = 0.0
	var hdZ = 0.0
	if keyDown(^left) then
		hdX = -1.0
		player.facingX = false
		player.facingY = 0
	elsif keyDown(^right) then
		hdX = 1.0
		player.facingX = true
		player.facingY = 0
	end
	if keyDown(^up) then
		hdZ = -1.0
		player.facingY = -1
	elsif keyDown(^down) then
		hdZ = 1.0
		player.facingY = 1
	end
	if keyPress(^z) then
		player.body.yspd = -0x600
		--player.body.yspd = -0x900
	end
	-- directionalize speed
	if hdX != 0.0 || hdZ != 0.0 then
		var mag = sqrt(hdX * hdX + hdZ * hdZ)
		hdX = hdX /. mag
		hdZ = hdZ /. mag
		player.body.xspd = floor(hdX * toFloat(_SPEED))
		player.body.zspd = floor(hdZ * toFloat(_SPEED))
	else
		player.body.xspd = 0
		player.body.zspd = 0
	end	
	-- target options
	if |world.target.obj| > 0 then
		if keyPress(^c) then
			player.mode = 1 - player.mode
		end
	end
end

fn dist(GameObj obj1, GameObj obj2) Int
	var b1 = obj1.body.hitbox
	var b2 = obj2.body.hitbox
	return floor(distPt3(
		toFloat(b1.centerX()), toFloat(b1.y), toFloat(b1.centerZ()),
		toFloat(b2.centerX()), toFloat(b2.y), toFloat(b2.centerZ())
	))
end

fn pcTarget(GameWorld world)
	var target = world.target
	-- set distance of "closest" target (if target exists)
	-- - if target exists, bias towards not changing targets
	var closestDist = fromTile(99)
	if |target.obj| > 0 then
		closestDist = max(fromTile(2), dist(world.player, target.obj[0]) - fromTile(2))
		-- de-select target if too far away
		if closestDist > fromTile(10) then
			target.obj /= 0
		end
	end
	-- check for new target
	for i < |world.objList| do
		var curObj = world.objList[i]
		var oType = curObj.cType
		if oType is EnemyCol then
			var d = dist(world.player, curObj)
			if d < fromTile(10) && d < closestDist then
				if |target.obj| = 0 then target.obj += curObj
				else target.obj[0] = curObj	end
				closestDist = d
			end
		end
	end
end

fn colCheck(GameWorld world, GameObj obj1, GameObj obj2)
	var cc1 = obj1.cType
	var cc2 = obj2.cType
	if cc1 is PlayerCol && cc2 is EnemyCol then
		world.flag.state = BattleFlag
		world.flag.args += obj2
		(*var camera = world.camera
		camera.mode = ZoomCam
		camera.focusList += obj2.body*)
	end
end

fn update(GameWorld world)
	-- physics update
	world.room.update()
	var wMode = world.mode
	if !(wMode is GameMode) then return end
	(*
		only executes in "game mode"
	*)
	-- player controls
	var player = world.player
	world.pcInput()
	-- target
	world.pcTarget()
	-- cam controls
	if keyPress(^x) then
		world.camera.toggleManMode()
	end
	if keyPress(^m) then
		world.miniMode = !world.miniMode
	end
	world.camera.update()
	-- collision checks
	var camMode = world.camera.mode
	if camMode is ManCam then
		var objList = world.objList
		for j < i - 1, i < |objList| do
			var obj1 = objList[i]
			var obj2 = objList[j]
			if obj1.body.hitbox.overlaps(obj2.body.hitbox) then
				world.colCheck(obj1, obj2)
				world.colCheck(obj2, obj1)
			end
		end
	end
	-- object sector update
	var newSX = toTile(player.body.hitbox.centerX()) / 30
	var newSY = toTile(player.body.hitbox.centerZ()) / 30
	-- if the new sector is on the edge
	if newSX <= world.sectorX || newSX >= world.sectorX + 3 ||
		newSY <= world.sectorY || newSY >= world.sectorY + 3
	then
		var newSectorX = min(newSX, world.curSX) - 1
		var newSectorY = min(newSY, world.curSY) - 1
		world.reloadSectors(newSectorX, newSectorY)
	end
	-- update sector history
	if newSX != world.curSX || newSY != world.curSY then
		world.curSX = newSX
		world.curSY = newSY
	end
end

fn draw(GameWorld world)
	(*var tx = floor(world.px)
	var tz = floor(world.pz)
	var elevation = world.room.elevation(tx, tz)*)
	--updateCamMat(-40, -px - 0.5, elevation + 2.8, -pz - 8.5)
	(*var player = world.player.body
	var tx = toFP(player.hitbox.centerX())
	var ty = toFP(player.hitbox.y)
	var tz = toFP(player.hitbox.centerZ())
	if world.camMode then
		updateCamMat(-40, -tx, -ty + 3.8, -tz - 5.5)
	else
		updateCamMat(-45, -tx, -ty + 8.0, -tz - 10.5)
	end*)
	var player = world.player.body
	var tx = toFP(player.hitbox.centerX())
	var tz = toFP(player.hitbox.centerZ())
	world.camera.use()
	world.room.draw(floor(tx), floor(tz))
	Sulfur.setSPFlag(1.0)
	for i < |world.objList| do
		world.objList[i].draw()
	end
	if |world.target.obj| > 0 then
		world.target.obj[0].drawTarget()
	end
	Sulfur.setSPFlag(0.0)
end

fn drawMini(GameWorld world)
	var player = world.player.body
	var tx = toTile(player.hitbox.centerX())
	var tz = toTile(player.hitbox.centerZ())
	world.room.drawMini(world.miniMode, tx, tz)
	-- show objects
	if !world.miniMode then
		for i < |world.objList| do
			var curObj = world.objList[i]
			var mx = toTile(curObj.body.hitbox.centerX()) - (tx - 60)
			var mz = toTile(curObj.body.hitbox.centerZ()) - (tz - 60)
			if mx >= 0 && mx < 120 && mz >= 0 && mz < 120 then
				Sulfur.draw(GBox(0xc04040, mx, mz, 1, 1))
			end
		end
	end
	Sulfur.draw(GText(sfont, 8, 8, 4, toString(world.camera.state.x)))
	Sulfur.draw(GText(sfont, 8, 16, 4, toString(world.camera.state.y)))
	Sulfur.draw(GText(sfont, 8, 24, 4, toString(world.camera.state.z)))
end