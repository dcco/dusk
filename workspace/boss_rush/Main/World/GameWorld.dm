
struct GameWorld{
	GameRoom room,
	1d[GameObj] objList,
	GameObj player
}

const _MAPS = new 2d[4, 1][
	0, 0, 45, 32
]

fn newWorld() GameWorld
	-- initialize room
	var mw = _MAPS[2, 0]
	var mh = _MAPS[3, 0]
	var room = newRoom(mw, mh)
	for i < mw, j < mh do
		var px = _MAPS[0, 0] + i
		var py = _MAPS[1, 0] + j
		var p = maps.pixel(px, py)
		if matchColor(p, 0b, 0b, 0b) then
			room.data[i, j].type = PGrass
		end
	end
	-- initialize player
	var player = newGameObj()
	var initList = new 1d[~GameObj]
	room.addObj(player.body)
	initList += player
	-- final return
	return new GameWorld{
		room = room,
		objList = initList,
		player = player
	}
end

fn pcInput(GameObj player)
	if keyDown(^left) then
		player.body.xspd = -0x300
		player.facing = false
	elsif keyDown(^right) then
		player.body.xspd = 0x300
		player.facing = true
	else
		player.body.xspd = 0
	end
	if keyPress(^z) && player.body.wall.bot then
		player.body.yspd = -0xC00
	end
end

fn update(GameWorld world)
	world.room.update()
	pcInput(world.player)
end

fn updateCamMat(Int angle, Float x, Float y, Float z)
	idMat4(RV.mvMat)
	RV.mvMat.rotateX(toRadians(angle))
	RV.mvMat.translate(x, y, z)
end

fn draw(GameWorld world)
	updateCamMat(-5, -0.0, -2.0, -16.5)
	world.room.draw()
	for i < |world.objList| do
		world.objList[i].draw()
	end
end