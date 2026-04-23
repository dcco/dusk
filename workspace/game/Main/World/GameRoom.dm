
struct GameRoom{
	Int width,
	Int height,
	2d[GameTile] grid
}

fn elevation(GameRoom room, Int cx, Int cy) Float
	if cx < 0 || cx >= room.width || cy < 0 || cy >= room.height then return 0.0 end
	return room.grid[cx, cy].elevation
end

fn draw(GameRoom room, Int cx, Int cy)
	for i < 30, j < 30 do
		var tx = cx + i - 15
		var ty = cy + j - 20
		if tx >= 0 && tx < room.width && ty >= 0 && ty < room.height then
			room.grid[tx, ty].draw(toFloat(tx), 0.0, toFloat(ty))
		end
	end
end