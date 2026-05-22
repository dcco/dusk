
struct GameWorld{
	GameRoom room,
	1d[GameObj] objList,
	GameObj player,
	Bool camMode
	(*Float px,
	Float pz*)
}

fn newWorld() GameWorld
	-- initialize room
	var room = generateRoom()
	-- initialize player
	var player = newGameObj(3, 7, 1)
	var initList = new 1d[~GameObj]
	room.addObj(player.body, tilePixel(300), tilePixel(520))
	initList += player
	-- final return
	return new GameWorld{
		room = room,
		objList = initList,
		player = player,
		camMode = false
		--px = 300.0,
		--pz = 520.0
	}
end

fn update(GameWorld world)
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
	else
	end
	if hdX != 0.0 || hdZ != 0.0 then
		var mag = sqrt(hdX * hdX + hdZ * hdZ)
		hdX = hdX /. mag
		hdZ = hdZ /. mag
		--player.body.xspd = floor(hdX * toFloat(0x150))
		--player.body.zspd = floor(hdZ * toFloat(0x150))
		player.body.xspd = floor(hdX * toFloat(0x500))
		player.body.zspd = floor(hdZ * toFloat(0x500))
	else
		player.body.xspd = 0
		player.body.zspd = 0
	end
	-- cam controls
	if keyPress(^z) then
		world.camMode = !world.camMode
	end
	-- physics update
	world.room.update()
end

fn updateCamMat(Int angle, Float x, Float y, Float z)
	idMat4(RV.mvMat)
	RV.mvMat.rotateX(toRadians(angle))
	RV.mvMat.translate(x, y, z)
end

fn draw(GameWorld world)
	(*var tx = floor(world.px)
	var tz = floor(world.pz)
	var elevation = world.room.elevation(tx, tz)*)
	--updateCamMat(-40, -px - 0.5, elevation + 2.8, -pz - 8.5)
	var player = world.player.body
	var tx = toFP(player.hitbox.centerX())
	var ty = toFP(player.hitbox.y)
	var tz = toFP(player.hitbox.centerZ())
	if world.camMode then
		updateCamMat(-40, -tx, -ty + 3.8, -tz - 5.5)
	else
		updateCamMat(-45, -tx, -ty + 8.0, -tz - 10.5)
	end
	world.room.draw(floor(tx), floor(tz))
	for i < |world.objList| do
		world.objList[i].draw()
	end
end